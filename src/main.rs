#![feature(proc_macro_hygiene)]
#[macro_use]
extern crate ndarray;
extern crate clap;
extern crate ndarray_parallel;
extern crate num;
extern crate phf;
extern crate terminal_size;
#[macro_use]
extern crate phf_macros;
extern crate hex;
extern crate image;

use clap::{App, Arg, ArgGroup};
use ndarray::{Array2, Axis, Zip};
use ndarray_parallel::prelude::*;
use num::complex::Complex;
use num::complex::*;
use terminal_size::{terminal_size, Height, Width};

/// ANSI background colour escapes.
pub static ANSI_BG_COLOUR_ESCAPES: &'static [&'static str] = &[
    "\x1B[40m", "\x1B[41m", "\x1B[42m", "\x1B[43m", "\x1B[44m", "\x1B[45m",
    "\x1B[46m", "\x1B[47m",
];
pub static ANSI_RESET_ATTRIBUTES: &str = "\x1B[0m";

#[derive(Clone, Copy, Debug)]
pub struct Color {
    r: u8,
    g: u8,
    b: u8,
}

impl Color {
    fn iter(&self) -> impl Iterator<Item = u8> {
        let r = std::iter::once(self.r);
        let b = std::iter::once(self.b);
        let g = std::iter::once(self.g);
        r.chain(b).chain(g)
    }

    fn to_image_rgb(&self) -> image::Rgb<u8> {
        image::Rgb([self.r, self.g, self.b])
    }
}

impl std::iter::FromIterator<u8> for Color {
    fn from_iter<I: IntoIterator<Item = u8>>(iter: I) -> Self {
        let mut iter_used = iter.into_iter();
        Color {
            r: iter_used.next().unwrap(),
            b: iter_used.next().unwrap(),
            g: iter_used.next().unwrap(),
        }
    }
}

pub fn write_ansi_truecolor<F>(magnitudes: &Array2<i32>, value_to_color: F)
where
    F: Fn(i32) -> Color,
{
    for axis_pair in magnitudes.axis_chunks_iter(Axis(1), 2) {
        for (value_upper, value_lower) in axis_pair
            .slice(s![.., 0])
            .iter()
            .zip(axis_pair.slice(s![.., 0]).iter())
        {
            let (upper, lower) =
                (value_to_color(*value_upper), value_to_color(*value_lower));
            print!(
                "\x1B[38;2;{};{};{}m\
                 \x1B[48;2;{};{};{}m\u{2580}", // ▀
                upper.r, upper.g, upper.b, lower.r, lower.g, lower.b
            )
        }
        println!("{}", ANSI_BG_COLOUR_ESCAPES[0])
    }
    println!("{}", ANSI_RESET_ATTRIBUTES);
}

pub fn write_image<F>(
    magnitudes: &Array2<i32>,
    value_to_color: F,
    path: &str,
) -> std::result::Result<(), std::io::Error>
where
    F: Fn(i32) -> Color,
{
    let shape = magnitudes.shape();
    let w = shape[0];
    let h = shape[1];
    let mut img_buf = image::ImageBuffer::new(w as u32, h as u32);

    for (value, pixel) in magnitudes.iter().zip(img_buf.pixels_mut()) {
        *pixel = value_to_color(*value).to_image_rgb();
    }
    img_buf.save(path)
}

pub fn mandelbrot(
    top_left: Complex32,
    bottom_right: Complex32,
    num_steps: Complex<usize>,
    threshold: f32,
    max_iters: i32,
) -> Array2<i32> {
    let diff = bottom_right - top_left;
    let slope_re = diff.re / num_steps.re as f32;
    let slope_im = diff.im / num_steps.im as f32;
    let mut out = Array2::<i32>::zeros((num_steps.re, num_steps.im));
    let threshold_squared = threshold.powi(2);
    Zip::indexed(&mut out).par_apply(|(index_re, index_im), iterations| {
        let c = Complex32::new(
            index_re as f32 * slope_re + top_left.re,
            index_im as f32 * slope_im + top_left.im,
        );
        let mut val = c.clone();
        while val.norm_sqr() < threshold_squared
            && iterations.clone() < max_iters
        {
            val = val * val + c;

            *iterations += 1;
        }
    });
    out
}

static COLOR_MAP: phf::Map<&'static str, Color> = phf_map! {
    "red" => Color {r: 255, g: 0, b: 0},
    "green" => Color {r: 0, g: 255, b: 0},
    "blue" => Color {r: 0, g: 0, b: 255},
    "white" => Color {r: 255, g: 255, b: 255},
    "purple" => Color {r: 128, g: 0, b: 128},
    "cyan" => Color {r: 0, g: 255, b: 255},
};

pub fn main() {
    let available_basic_colors = COLOR_MAP
        .keys()
        .map(|s| &**s)
        .collect::<Vec<&str>>()
        .join(", ");
    let size = terminal_size();

    let bounds_name = "bounds";
    let bounds_default = "-2.5,1.0,-2.0,2.0";
    let zoom_name = "zoom";
    let zoom_start_bound_name = "zoom_start_bound";
    let zoom_start_bound_default = "0.0,1.0,0.0,1.0";
    let zoom_end_bound_name = "zoom_end_bound";
    let zoom_end_bound_default = bounds_default;
    let zoom_fps_name = "zoom_fps";
    let zoom_fps_default = "10";
    let zoom_time_name = "zoom_time";
    let zoom_time_default = "5";
    let color_basic_name = "color";
    let color_basic_default = "blue";
    let color_range_name = "color_range";
    let color_range_default = "000000,0000FF";
    let image_name = "image";
    let image_dimensions_name = "image_dimensions";
    let image_dimensions_default = "1920,1080";
    let image_path_name = "image_path";
    let image_path_default = "mandelbrot.png";

    let construct_arg = |name, help, short, value_name_op| {
        let arg = Arg::with_name(name).long(name).help(help);
        let arg_if_takes_value = match value_name_op {
            Some(value_name) => arg.takes_value(true).value_name(value_name),
            None => arg,
        };
        if short {
            arg_if_takes_value.short(name[0..1].to_string())
        } else {
            arg_if_takes_value
        }
    };

    let matches = App::new("mandelbrot")
        .version("0.1.0")
        .author("Ryan Greenblatt <ryan_greenblatt@brown.com>")
        .about("Generates and displays the mandelbrot set.")
        .set_term_width(if let Some((Width(w), _)) = size {
            w as usize
        } else {
            120
        })
        .groups(&[
            ArgGroup::with_name("bounding").args(&[bounds_name, zoom_name]),
            ArgGroup::with_name("color_group").args(&[color_basic_name, color_range_name]),
        ])
        .arg(construct_arg(
            bounds_name,
            &format!("Sets the bounds of the \
             region\nOrder: real min, real max, imaginary min, imaginary max\n\
             Default: {}", bounds_default),
            true,
            Some(&bounds_name.to_uppercase()),
        ))
        .arg(construct_arg(
            zoom_name,
            "Enables zooming from one bound to another bound",
            true,
            None,
        ))
        .arg(construct_arg(
            zoom_start_bound_name,
            &format!("Start zoom bound\nDefault: {}", zoom_start_bound_default),
            false,
            Some(&zoom_start_bound_name.to_uppercase()),
        ))
        .arg(construct_arg(
            zoom_end_bound_name,
            &format!("End zoom bound\nDefault: {}", zoom_end_bound_default),
            false,
            Some(&zoom_end_bound_name.to_uppercase()),
        ))
        .arg(construct_arg(
            zoom_fps_name,
            &format!(
                "Frames per second used for zooming\nDefault: {}",
                zoom_fps_default
            ),
            false,
            Some(&zoom_fps_name.to_uppercase()),
        ))
        .arg(construct_arg(
            zoom_time_name,
            &format!(
                "Total time spent zooming\nDefault: {}",
                zoom_time_default
            ),
            false,
            Some(&zoom_time_name.to_uppercase()),
        ))
        .arg(construct_arg(
            image_name,
            "Save to an image instead of terminal output",
            true,
            None,
        ))
        .arg(construct_arg(
            image_dimensions_name,
            &format!("Image dimensions\nDefault: {}", image_dimensions_default),
            false,
            Some(&image_dimensions_name.to_uppercase()),
        ))
        .arg(construct_arg(
            image_path_name,
            &format!("Image save path\nDefault: {}", image_path_default),
            false,
            Some(&image_path_name.to_uppercase()),
        ))
        .arg(construct_arg(
            color_basic_name,
            &format!(
                "Sets the color to use\nAvailable colors: {}\nDefault: {}",
                available_basic_colors, color_basic_default
            ),
            true,
            Some(&color_basic_name.to_uppercase()),
        ))
        .arg(construct_arg(
            color_range_name,
            &format!(
                "Sets the color to go between the two hex color codes specified\
                 \nDefault: {}",
                color_range_default
            ),
            false,
            Some(&color_range_name.to_uppercase()),
        ))
        .get_matches();

    macro_rules! exit_error {
        ($($arg:tt)*) => ({
        eprintln!($($arg)*);
        std::process::exit(1);
        })
    }

    let default_min_color = Color { r: 1, g: 1, b: 1 };

    let (min_color, max_color) = match matches.value_of(color_range_name) {
        Some(v) => {
            let colors = v
                .split(",")
                .enumerate()
                .map(|(index, s)| match hex::decode(s) {
                    Ok(v) => {
                        if v.len() != 3 {
                            exit_error!(
                                "Invalid length of hex color code for \
                                 {} at index {}. Should be 3 bytes instead \
                                 of {}",
                                color_range_name,
                                index,
                                v.len()
                            );
                        } else {
                            Color {
                                r: v[0],
                                g: v[1],
                                b: v[2],
                            }
                        }
                    }
                    Err(e) => {
                        exit_error!(
                            "Failed to parse hex color code for color_range \
                             at index {} with error: {}",
                            index,
                            e
                        );
                    }
                })
                .collect::<Vec<Color>>();
            if colors.len() != 2 {
                exit_error!(
                    "Invalid color_range : expected 2 comma separated \
                     values, got {}",
                    colors.len()
                );
            }
            (colors[0], colors[1])
        }
        None => match COLOR_MAP.get(
            matches
                .value_of(color_basic_name)
                .unwrap_or(color_basic_default),
        ) {
            Some(v) => (default_min_color, *v),
            None => {
                exit_error!(
                    "Invalid color provided. Should be one of {}",
                    available_basic_colors
                );
            }
        },
    };

    let max_iters = 100;
    let threshold = 2.0;

    let convert_float = |v| v as f32;
    let color_slope = max_color
        .iter()
        .map(convert_float)
        .zip(min_color.iter().map(convert_float))
        .map(|(max, min)| max - min)
        .collect::<Vec<f32>>();
    let value_to_color = |value| {
        if value == 0 {
            min_color
        } else if value == max_iters {
            max_color
        } else {
            let frac = value as f32 / max_iters as f32;
            color_slope
                .iter()
                .zip(min_color.iter())
                .map(|(color_slope_v, min_color_v)| {
                    ((color_slope_v * frac) as i32 + min_color_v as i32) as u8
                })
                .collect::<Color>()
        }
    };
    let compute_mandelbrot_general =
        |general_top_left: Complex32,
         general_bottom_right: Complex32,
         w: usize,
         h: usize| {
            let dimensions_ratio = w as f32 / h as f32;

            let diff_re = general_bottom_right.re - general_top_left.re;
            let diff_im = general_top_left.im - general_bottom_right.im;
            let ratio = diff_re / diff_im;

            let (top_left, bottom_right) = if dimensions_ratio > ratio {
                let diff_re_delta_each =
                    (diff_im * dimensions_ratio - diff_re) / 2.0;
                assert!(diff_re_delta_each >= 0.0);
                (
                    Complex32::new(
                        general_top_left.re - diff_re_delta_each,
                        general_top_left.im,
                    ),
                    Complex32::new(
                        general_bottom_right.re + diff_re_delta_each,
                        general_bottom_right.im,
                    ),
                )
            } else {
                let diff_im_delta_each =
                    (diff_re / dimensions_ratio - diff_im) / 2.0;
                assert!(diff_im_delta_each >= 0.0);

                (
                    Complex32::new(
                        general_top_left.re,
                        general_top_left.im + diff_im_delta_each,
                    ),
                    Complex32::new(
                        general_bottom_right.re,
                        general_bottom_right.im - diff_im_delta_each,
                    ),
                )
            };

            return mandelbrot(
                top_left,
                bottom_right,
                Complex::<usize>::new(w, h),
                threshold,
                max_iters,
            );
        };

    let render_terminal = |general_top_left: Complex32,
                           general_bottom_right: Complex32,
                           w: u16,
                           h: u16| {
        write_ansi_truecolor(
            &compute_mandelbrot_general(
                general_top_left,
                general_bottom_right,
                w as usize,
                h as usize * 2,
            ),
            value_to_color,
        )
    };

    let render_image = |general_top_left: Complex32,
                        general_bottom_right: Complex32,
                        w: u32,
                        h: u32,
                        path: &str| {
        match write_image(
            &compute_mandelbrot_general(
                general_top_left,
                general_bottom_right,
                w as usize,
                h as usize,
            ),
            value_to_color,
            path,
        ) {
            Ok(_) => (),
            Err(e) => exit_error!("Failed to save image with error: {}", e),
        }
    };

    fn parse_comma_separated<T>(
        arg_name: &str,
        default: &str,
        num: usize,
        matches: &clap::ArgMatches,
    ) -> Vec<T>
    where
        T: std::str::FromStr,
        <T as std::str::FromStr>::Err: std::fmt::Display,
    {
        let values = matches
            .value_of(arg_name)
            .unwrap_or(default)
            .split(",")
            .enumerate()
            .map(|(index, s)| match s.parse::<T>() {
                Ok(v) => v,
                Err(e) => exit_error!(
                    "Failed to parse value (for arg {}) at index {} with error: {}",
                    arg_name,
                    index,
                    e
                ),
            })
            .collect::<Vec<T>>();

        if values.len() != num {
            exit_error!(
                "Invalid number of values (for arg {}): expected {}, got {}",
                arg_name,
                num,
                values.len()
            );
        }
        values
    }

    let parse_dimension = |arg_name, default| {
        let dimension_values =
            parse_comma_separated(arg_name, default, 2, &matches);
        (dimension_values[0], dimension_values[1])
    };

    let (w_term, h_term, w_image, h_image, path);
    let render: Box<dyn Fn(Complex32, Complex32) -> ()> = if matches
        .is_present("image")
    {
        let (w_found, h_found) =
            parse_dimension(image_dimensions_name, image_dimensions_default);
        w_image = w_found;
        h_image = h_found;
        path = matches
            .value_of(image_path_name)
            .unwrap_or(image_path_default);
        Box::new(
            |general_top_left: Complex32, general_bottom_right: Complex32| {
                render_image(
                    general_top_left,
                    general_bottom_right,
                    w_image,
                    h_image,
                    path,
                )
            },
        )
    } else if let Some((Width(w_found), Height(h_found))) = size {
        w_term = w_found;
        h_term = h_found;
        Box::new(
            |general_top_left: Complex32, general_bottom_right: Complex32| {
                render_terminal(
                    general_top_left,
                    general_bottom_right,
                    w_term,
                    h_term,
                )
            },
        )
    } else {
        exit_error!("Not connected to tty?");
    };

    let parse_bound = |arg_name, default| {
        let bound_values =
            parse_comma_separated(arg_name, default, 4, &matches);
        (
            Complex32::new(bound_values[0], bound_values[3]),
            Complex32::new(bound_values[1], bound_values[2]),
        )
    };

    let parse_float = |arg_name, default| match matches
        .value_of(arg_name)
        .unwrap_or(default)
        .parse::<f32>()
    {
        Ok(v) => v,
        Err(e) => exit_error!(
            "Failed to parse float (for arg {}) with error: {}",
            arg_name,
            e
        ),
    };

    let render_zoom =
        |start_general_top_left: Complex32,
         start_general_bottom_right: Complex32,
         end_general_top_left: Complex32,
         end_general_bottom_right: Complex32| {
            let diff_general_top_left =
                end_general_top_left - start_general_top_left;
            let diff_general_bottom_right =
                end_general_bottom_right - start_general_bottom_right;

            let zoom_time = parse_float(zoom_time_name, zoom_time_default);
            let zoom_fps = parse_float(zoom_fps_name, zoom_fps_default);
            let time = 1.0 / zoom_fps;
            let duration =
                std::time::Duration::from_millis((time * 1000.0) as u64);

            let iters = (zoom_time * zoom_fps) as i32;
            for iter in 0..iters {
                let frac = iter as f32 / iters as f32;
                let general_top_left =
                    start_general_top_left + diff_general_top_left * frac;
                let general_bottom_right = start_general_bottom_right
                    + diff_general_bottom_right * frac;
                render(general_top_left, general_bottom_right);
                std::thread::sleep(duration);
            }
        };

    if matches.is_present(zoom_name) {
        let (start_general_top_left, start_general_bottom_right) =
            parse_bound(zoom_start_bound_name, zoom_start_bound_default);
        let (end_general_top_left, end_general_bottom_right) =
            parse_bound(zoom_end_bound_name, zoom_end_bound_default);
        render_zoom(
            start_general_top_left,
            start_general_bottom_right,
            end_general_top_left,
            end_general_bottom_right,
        );
    } else {
        let (general_top_left, general_bottom_right) =
            parse_bound(bounds_name, bounds_default);

        render(general_top_left, general_bottom_right);
    }
}
