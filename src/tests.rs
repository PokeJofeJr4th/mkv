use rand::{prelude::Distribution, thread_rng};

use crate::{lazy_mkv, MarkovError};

use super::MarkovData;

#[test]
fn byte_to_char() {
    use super::byte_to_char;
    assert_eq!(byte_to_char(0b1110_0001), Some(('a', 8)));
    assert_eq!(byte_to_char(0b0000_0001), Some(('a', 1)));
    assert_eq!(byte_to_char(0b0001_1010), Some(('z', 1)));
    assert_eq!(byte_to_char(0b0111_1011), Some((';', 4)));
    assert_eq!(byte_to_char(0b0111_1100), None);
    assert_eq!(byte_to_char(0b0000_0000), None);
}

#[test]
fn char_to_byte() {
    use super::char_to_byte;
    assert_eq!(char_to_byte(('a', 8)), Ok(0b1110_0001));
    assert_eq!(char_to_byte(('z', 1)), Ok(0b0001_1010));
    assert_eq!(char_to_byte((';', 4)), Ok(0b0111_1011));

    assert_eq!(
        char_to_byte(('`', 7)),
        Err(MarkovError::UnsupportedCharacter('`'))
    );
    assert_eq!(
        char_to_byte((' ', 7)),
        Err(MarkovError::UnsupportedCharacter(' '))
    );
}

#[test]
fn inverse_function() {
    use super::{byte_to_char, char_to_byte};
    assert_eq!(
        char_to_byte(byte_to_char(0b1110_0001).unwrap()),
        Ok(0b1110_0001)
    );
    assert_eq!(
        byte_to_char(char_to_byte(('a', 4)).unwrap()),
        Some(('a', 4))
    );
    assert_eq!(
        char_to_byte(byte_to_char(0b1111_1011).unwrap()),
        Ok(0b1111_1011)
    );
    assert_eq!(
        byte_to_char(char_to_byte((';', 4)).unwrap()),
        Some((';', 4))
    );
    assert_eq!(
        byte_to_char(char_to_byte(('z', 40)).unwrap()),
        Some(('z', 8))
    );
}

#[test]
fn save_and_load() {
    let mut rng = thread_rng();
    let string_pool = ["strings"];

    let initialized_markov = MarkovData::from_strings(&string_pool);
    let bytes = initialized_markov.to_bytes().unwrap();

    let loaded_markov = MarkovData::from_bytes(&bytes).unwrap();
    let test_sample: String = loaded_markov.sample(&mut rng);
    assert_eq!(String::from("Strings"), test_sample);
}

#[test]
fn capitalization_invariant() {
    let mut rng = thread_rng();

    let capital_pool = ["STRINGS"];
    let capital_markov = MarkovData::from_strings(&capital_pool);
    let capital_sample: String = capital_markov.sample(&mut rng);

    let lowercase_pool = ["strings"];
    let lowercase_markov = MarkovData::from_strings(&lowercase_pool);
    let lowercase_sample: String = lowercase_markov.sample(&mut rng);

    assert_eq!(capital_sample, lowercase_sample);
}

#[test]
fn test_lazy_mkv() {
    let mkv = lazy_mkv!(["STRINGS"]);

    let lazy_sample: String = mkv.sample(&mut thread_rng());

    assert_eq!(lazy_sample, "Strings");
}
