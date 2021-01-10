use kanji::is_katakana;

use std::env::args;
use std::fs::File;
use std::io::BufReader;

use xml::reader::{EventReader, XmlEvent};

// SIGBOVIK logic

fn process_word(readings: &[String], meanings: &[String]) {
    // TODO: do the thing w/essential consonants
    // Probably good to strip out parentheses in the definitions
    // TODO: allow morphological suffixes to be dropped, eg "ソロキャンプ" -> "solo campING"
    // This probably requires a special case whitelist otws it makes the essential-char logic
    // vacuous lol
    println!("word: {:?} => {:?}", readings, meanings);
}

// Misc. japanese stuff

fn is_katakana_word(word: &str) -> bool {
    let mut nonempty = false;
    // TODO: Might need to add stuff like wi, we, hiragana repetition marks?
    // Otoh, essential-consonants filter logic might subsume that.
    for c in word.chars() {
        nonempty = true;
        // This just tests if it's between u30A0 and u30FF.
        if !is_katakana(&c) {
            return false;
        }
    }
    nonempty
}

// XML stuff

fn expect_chars(parser: &mut impl Iterator<Item = xml::reader::Result<XmlEvent>>, msg: &str) -> String {
    match parser.next().expect("xml stream ended").expect("parse xml") {
        XmlEvent::Characters(s) => s,
        e => panic!("expected characters, got {:?}, {}", e, msg),
    }
}

fn main() {
    let filename = args().skip(1).next().expect("expected a filename as first arg");
    let file = File::open(filename).expect("open file");
    // let mut contents = String::new();
    // file.read_to_string(&mut contents).expect("read file");
    let mut parser = EventReader::new(BufReader::new(file)).into_iter();

    let mut cur_word_rebs = vec![];
    let mut cur_word_meanings = vec![];
    let mut cur_word_katakana_only = true;

    while let Some(e) = parser.next() {
        match e {
            Ok(XmlEvent::StartElement { name, .. }) => {
                if name.local_name == "entry" {
                    assert!(cur_word_rebs.is_empty(), "nested entry?");
                } else if name.local_name == "reb" {
                    // Phonetic reading for the word.
                    let reb = expect_chars(&mut parser, "reb text");
                    if is_katakana_word(&reb) {
                        cur_word_rebs.push(reb);
                    } else {
                        cur_word_katakana_only = false;
                    }
                } else if name.local_name == "keb" {
                    // Kanji (or roman alphabet) rendering of the word.
                    cur_word_katakana_only = false;
                } else if name.local_name == "gloss" {
                    cur_word_meanings.push(expect_chars(&mut parser, "gloss text"));
                }
            },
            Ok(XmlEvent::EndElement { name, .. }) => {
                if name.local_name == "entry" {
                    if cur_word_katakana_only {
                        process_word(&cur_word_rebs, &cur_word_meanings);
                    }
                    cur_word_rebs.clear();
                    cur_word_meanings.clear();
                    cur_word_katakana_only = true;
                }
            },
            Ok(_) => {},
            // NB: Had to preprocess JMDict with 's/>&/>' because it has a lot of
            // e.g. "&unc;", "&ateji;", etc tags, which make the xml parser choke,
            // bc it expects e.g. &amp; &lt; &gt, etc after ampersands only.
            Err(e) => println!("error parsing: {:?}", e),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_kata() {
        assert!(is_katakana_word("ファルミナスエッジ"));
        assert!(is_katakana_word("ー"));
        assert!(is_katakana_word("・"));

        assert!(!is_katakana_word(""));
        assert!(!is_katakana_word("ａｂｃ"));
        assert!(!is_katakana_word("１２３"));
        assert!(!is_katakana_word("こんにちは"));
    }
}
