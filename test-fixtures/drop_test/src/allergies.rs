pub struct Allergies {
    allergy_score: u8,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Allergen {
    Eggs,
    Peanuts,
    Shellfish,
    Strawberries,
    Tomatoes,
    Chocolate,
    Pollen,
    Cats,
}

static ALL_ALLERGENS: [Allergen; 8] = [
    Allergen::Eggs,
    Allergen::Peanuts,
    Allergen::Shellfish,
    Allergen::Strawberries,
    Allergen::Tomatoes,
    Allergen::Chocolate,
    Allergen::Pollen,
    Allergen::Cats,
];

impl Allergies {
    pub fn new(score: u32) -> Self {
        Self {
            allergy_score: score as u8,
        }
    }

    pub fn is_allergic_to(&self, allergen: &Allergen) -> bool {
        let bit = 1 << (*allergen as u8);
        self.allergy_score & bit != 0
    }

    pub fn allergies(&self) -> Vec<Allergen> {
        ALL_ALLERGENS
            .iter()
            .filter(|a| self.is_allergic_to(a))
            .copied()
            .collect()
    }
}
