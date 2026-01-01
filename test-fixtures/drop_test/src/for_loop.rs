/// Test for-loop iteration variable Copy detection.
/// The iteration variable (group_size: usize) should be detected as Copy.
pub fn test_for_loop_copy() {
    let max_group = 5usize;
    for group_size in 1..=max_group {
        take(group_size);  // group_size is usize, should show as "copied"
    }
}

fn take(_x: usize) {}

/// Test call-site with mixed argument types.
/// Should show both Copy (book_counts) and &mut borrow (cache).
pub fn test_call_site_borrows() {
    use std::collections::HashMap;
    let book_counts: [u32; 5] = [0; 5];
    let mut cache: HashMap<u32, u32> = HashMap::new();
    find_min(book_counts, &mut cache);
}

fn find_min(_counts: [u32; 5], _cache: &mut std::collections::HashMap<u32, u32>) -> u32 {
    0
}

/// Test state panel shows variables after a for loop.
/// The state panel should show `cache` even when `book_counts` is Copy-filtered.
pub fn lowest_price(books: &[u32]) -> u32 {
    let mut book_counts: [u32; 5] = [0; 5];
    for &book in books {
        book_counts[(book - 1) as usize] += 1;
    }

    let mut cache: std::collections::HashMap<[u32; 5], u32> = std::collections::HashMap::new();
    find_min2(book_counts, &mut cache)
}

fn find_min2(_counts: [u32; 5], _cache: &mut std::collections::HashMap<[u32; 5], u32>) -> u32 {
    0
}
