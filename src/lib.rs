#[macro_use]
extern crate ndarray;
extern crate image;
extern crate ndarray_parallel;
extern crate num;

pub use derive::{Color, write_ansi_truecolor, write_image, compute_mandelbrot};

pub mod derive {
    use ndarray::{Array2, Axis, Zip};
    use ndarray_parallel::prelude::*;
    use num::complex::{Complex, Complex32};

    /// ANSI background colour escapes.
    static ANSI_BG_COLOUR_ESCAPES: &'static [&'static str] = &[
        "\x1B[40m", "\x1B[41m", "\x1B[42m", "\x1B[43m", "\x1B[44m", "\x1B[45m",
        "\x1B[46m", "\x1B[47m",
    ];
    static ANSI_RESET_ATTRIBUTES: &str = "\x1B[0m";

    #[derive(Clone, Copy, Debug)]
    pub struct Color {
        pub r: u8,
        pub g: u8,
        pub b: u8,
    }

    impl Color {
        pub fn iter(&self) -> impl Iterator<Item = u8> {
            let r = std::iter::once(self.r);
            let b = std::iter::once(self.b);
            let g = std::iter::once(self.g);
            r.chain(b).chain(g)
        }

        pub fn to_image_rgb(&self) -> image::Rgb<u8> {
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
        for axis_pair in magnitudes.axis_chunks_iter(Axis(0), 2) {
            for (value_upper, value_lower) in axis_pair
                .slice(s![0, ..])
                .iter()
                .zip(axis_pair.slice(s![1, ..]).iter())
            {
                let (upper, lower) = (
                    value_to_color(*value_upper),
                    value_to_color(*value_lower),
                );
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
        let w = shape[1];
        let h = shape[0];
        let mut img_buf = image::ImageBuffer::new(w as u32, h as u32);

        for (value, pixel) in magnitudes.iter().zip(img_buf.pixels_mut()) {
            *pixel = value_to_color(*value).to_image_rgb();
        }
        img_buf.save(path)
    }

    pub fn compute_mandelbrot(
        top_left: Complex32,
        bottom_right: Complex32,
        num_steps: Complex<usize>,
        threshold: f32,
        max_iters: i32,
    ) -> Array2<i32> {
        let diff = bottom_right - top_left;
        let slope_re = diff.re / num_steps.re as f32;
        let slope_im = diff.im / num_steps.im as f32;
        let mut out = Array2::<i32>::zeros((num_steps.im, num_steps.re));
        let threshold_squared = threshold.powi(2);
        Zip::indexed(&mut out).par_apply(|(index_im, index_re), iterations| {
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

} /* mandelbrot */
