pub fn classify(n: i32) -> &'static str {
    if n < 0 {
        "negative"
    } else if n == 0 {
        "zero"
    } else if n < 10 {
        "small"
    } else {
        "large"
    }
}

pub fn clamp_add(a: u8, b: u8) -> u8 {
    if (a as u16) + (b as u16) > 255 {
        u8::MAX
    } else {
        a + b
    }
}

pub fn never_tested() -> u32 {
    let mut t = 0;
    for i in 0..3 {
        t += i;
    }
    t
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_negative() {
        assert_eq!(classify(-1), "negative");
    }

    #[test]
    fn example_zero() {
        assert_eq!(classify(0), "zero");
    }

    #[test]
    fn prop_small_large() {
        let mut n = 1;
        while n < 50 {
            assert!(matches!(classify(n), "small" | "large"));
            n += 1;
        }
    }

    #[test]
    fn example_clamp_ok() {
        assert_eq!(clamp_add(1, 2), 3);
    }

    #[test]
    fn prop_clamp_saturates() {
        assert_eq!(clamp_add(250, 10), 255);
    }
}
