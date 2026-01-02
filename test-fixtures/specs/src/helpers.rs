//! Helper types for testing parameter last-use detection.

pub struct Allergies {
    score: u8,
}

impl Allergies {
    pub fn new(score: u32) -> Self {
        Self {
            score: score as u8,  // score param used here, not at declaration
        }
    }

    pub fn score(&self) -> u8 {
        self.score
    }
}
