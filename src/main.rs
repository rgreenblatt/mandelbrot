#![feature(proc_macro_hygiene)]
#[macro_use]
extern crate ndarray;
extern crate ndarray_parallel;
extern crate num;
extern crate terminal_size;
#[macro_use]
extern crate clap;
extern crate phf;
#[macro_use]
extern crate phf_macros;
extern crate hex;

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
    Zip::indexed(&mut out).par_apply(|(index_re, index_im), iterations| {
        let c = Complex32::new(
            index_re as f32 * slope_re + top_left.re,
            index_im as f32 * slope_im + top_left.im,
        );
        let mut val = c.clone();
        while val.norm() < threshold && iterations.clone() < max_iters {
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
    let color_basic_help = &format!(
        "Sets the color to use\nAvailable colors: {}\nDefault: blue",
        available_basic_colors
    );
    let size = terminal_size();
    let matches = clap_app!(myapp =>
                            (set_term_width: (if let Some((Width(w), _)) = size { w as usize } else { 120 }))
        (version: "0.1.0")
        (author: "Ryan Greenblatt <ryan_greenblatt@brown.com>")
        (about: "Generates and displays the mandelbrot set.")
        (@group bounding =>
            (@arg bounds: -b --bounds +takes_value "Sets the bounds of the \
             region\nOrder: real min, real max, imaginary min, imaginary max\n\
             Default: -2.5,1.0,-2.0,2.0")
            (@arg zoom: -z --zoom "Enables zooming from one bound to\
             another bound")
         )
        (@arg zoom_start_bound: --zoom_start_bound +takes_value requires[zoom] "Start zoom bound\nDefault: 0,1,0,1")
        (@arg zoom_end_bound: --zoom_end_bound +takes_value requires[zoom] "End zoom bound\nDefault: -2.5,1.0,-2.0,2.0")
        (@arg zoom_fps: --zoom_fps +takes_value requires[zoom] "Frames per second used for zooming\nDefault: 10")
        (@arg zoom_time: --zoom_time +takes_value requires[zoom] "Total time spent zooming (s)\nDefault: 5")
        (@group color =>
         (@arg color_basic: -c --color +takes_value color_basic_help)
         (@arg color_range: --color_range +takes_value "Sets the color to go \
          between the two hex color codes specified\nDefault:000000,0000FF")
        )
    )
    .get_matches();

    macro_rules! exit_error {
        ($($arg:tt)*) => ({
        eprintln!($($arg)*);
        std::process::exit(1);
        })
    }

    let default_min_color = Color { r: 1, g: 1, b: 1 };

    let (min_color, max_color) = match matches.value_of("color_range") {
        Some(v) => {
            let colors = v
                .split(",")
                .enumerate()
                .map(|(index, s)| match hex::decode(s) {
                    Ok(v) => {
                        if v.len() != 3 {
                            exit_error!(
                                "Invalid length of hex color code for color_range \
                                 at index {}. Should be 3 bytes instead of {}",
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
                    "Invalid color_range : expected 2 comma separated values, got {}",
                    colors.len()
                );
            }
            (colors[0], colors[1])
        }
        None => match COLOR_MAP
            .get(matches.value_of("color_basic").unwrap_or("blue"))
        {
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
        let frac = value as f32 / max_iters as f32;
        color_slope
            .iter()
            .zip(min_color.iter())
            .map(|(color_slope_v, min_color_v)| {
                ((color_slope_v * frac) as i32 + min_color_v as i32) as u8
            })
            .collect::<Color>()
    };

    let render_terminal = |general_top_left: Complex32,
                           general_bottom_right: Complex32,
                           w: u16,
                           h: u16| {
        let effective_h = h as usize * 2;
        let term_ratio = w as f32 / effective_h as f32;

        let diff_re = general_bottom_right.re - general_top_left.re;
        let diff_im = general_top_left.im - general_bottom_right.im;
        let ratio = diff_re / diff_im;

        let (top_left, bottom_right) = if term_ratio > ratio {
            let diff_re_delta_each = (diff_im * term_ratio - diff_re) / 2.0;
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
            let diff_im_delta_each = (diff_re / term_ratio - diff_im) / 2.0;
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

        let result = mandelbrot(
            top_left,
            bottom_right,
            Complex::<usize>::new(w as usize, h as usize * 2),
            threshold,
            max_iters,
        );

        write_ansi_truecolor(&result, value_to_color);
    };

    let (w, h);
    let render = if let Some((Width(w_found), Height(h_found))) = size {
        w = w_found;
        h = h_found;
        |general_top_left: Complex32, general_bottom_right: Complex32| {
            render_terminal(general_top_left, general_bottom_right, w, h)
        }
    } else {
        exit_error!("Not connected to tty?");
    };

    let parse_bound = |arg_name, default| {
        let bound_values = matches
            .value_of(arg_name)
            .unwrap_or(default)
            .split(",")
            .enumerate()
            .map(|(index, s)| match s.parse::<f32>() {
                Ok(v) => v,
                Err(e) => exit_error!(
                    "Failed to parse bound (for arg {}) at index {} with error: {}",
                    arg_name,
                    index,
                    e
                ),
            })
            .collect::<Vec<f32>>();

        if bound_values.len() != 4 {
            exit_error!(
                "Invalid number of bounds (for arg {}): expected 4, got {}",
                arg_name,
                bound_values.len()
            );
        }
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

            let zoom_time = parse_float("zoom_time", "5.0");
            let zoom_fps = parse_float("zoom_fps", "10.0");
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

    if matches.is_present("zoom") {
        let (start_general_top_left, start_general_bottom_right) =
            parse_bound("zoom_start_bound", "0,1,0,1");
        let (end_general_top_left, end_general_bottom_right) =
            parse_bound("zoom_end_bound", "-2.5,1.0,-1.2,1.2");
        render_zoom(
            start_general_top_left,
            start_general_bottom_right,
            end_general_top_left,
            end_general_bottom_right,
        );
    } else {
        let (general_top_left, general_bottom_right) =
            parse_bound("bounds", "-2.5,1.0,-1.2,1.2");

        render(general_top_left, general_bottom_right);
    }
}
