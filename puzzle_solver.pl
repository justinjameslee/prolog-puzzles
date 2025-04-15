:- module(puzzle_solver, [puzzle_solution/2]).
:- use_module(library(clpfd), [transpose/2]).
:- use_module(library(plunit)).

/*********************************************************************
 * Author:   Justin Lee <justinlee@student.unimelb.edu.au> <1599455>
 * Purpose:  Fill-in Puzzle Solver that given a crossword grid and a 
 *           word list, fills the grid with the words from the list.
 *
 * This module provides a solution to crossword puzzles by leveraging
 * a dynamic algorithm approach known as Minimum Remaining Values (MRV)
 * heuristic — also known as the "most constrained variable first" 
 * strategy. In essence, the algorithm identifies the slot with the fewest
 * possible candidates and attempts to fill it first.
 *
 * Key Constraints:
 *   - Each word from the WordList must be used exactly once.
 *   - All pre-filled letters must be respected.
 *   - Words in the WordList must have at least two letters.
 *   - Valid slots must be at least two cells long.
 *
 * Strategy:
 *   - Create a dictionary that groups words by their lengths.
 *   - Extract all horizontal and vertical slots of length ≥ 2.
 *   - Recursively fill the puzzle using a constraint-driven search,
 *     always selecting the most constrained slot first (MRV heuristic).
 *
 * Note:
 *   - Once a word is placed in a slot, remove it from the dictionary
 *     and proceed to fill the remaining slots.
 *   - If a slot cannot be filled, backtrack and try the next candidate.
 *   - The puzzle is considered solved when all slots are successfully filled
 *     and the WordList is empty.
 *
 * Output:
 *   The Puzzle with all empty cells filled using words from the WordList.
 *********************************************************************/

/*********************************************************************
 * puzzle_solution/2
 * puzzle_solution(+Puzzle, +WordList)
 * The main entry point of the crossword solver.
 *
 * Arguments:
 *   Puzzle: A list-of-lists representing the crossword grid.
 *           Each cell is one of:
 *              - '#' (a blocked cell),
 *              - a lowercase letter (a pre-filled cell), or
 *              - an unbound variable (_) representing an empty cell.
 *   WordList: list of words, each word is a list of lowercase letters.
 *********************************************************************/
puzzle_solution(Puzzle, WordList) :-
    build_word_dict(WordList, WordDict0),
    extract_all_slots(Puzzle, Slots),
    fill_puzzle(Puzzle, Slots, WordDict0).

/*********************************************************************
 * build_word_dict/2
 * build_word_dict(+WordList, -WordDict)
 * Create a dictionary list of pairs (Length-[Words]) from the given WordList.
 *
 * Arguments:
 *   WordList: The list of words to organise
 *   WordDict: The resulting dictionary as a list of pairs (Length-[Words])
 *
 * Example: If WordList = [[c,a,t],[b,a,g],[i,t],[i,f]]
 *   Then WordDict will be:
 *   [
 *     2-[[i,t],[i,f]],
 *     3-[[c,a,t],[b,a,g]]
 *   ]
 *
 * Note:
 *   - We can later look up candidate words by length with
 *     'get_words_of_length/3'.
 *********************************************************************/
build_word_dict(WordList, WordDict) :-
    group_words_by_length(WordList, [], WordDict),
    debug(puzzle, 'Word Dictionary: ~w', [WordDict]).

/*********************************************************************
 * group_words_by_length/3
 * group_words_by_length(+WordList, +Acc, -Out)
 * Groups words by their length recursively
 *
 * Arguments:
 *   WordList: Remaining words to be processed, 
               Split into Word and RemainingWords
 *   Acc: Accumulator of grouped words so far
 *   Out: Final grouped dictionary
 *
 * Base Case: When WordList is empty, return the accumulated dictionary.
 *********************************************************************/
group_words_by_length([], Acc, Acc).
group_words_by_length([Word|RemainingWords], Acc, Out) :-
    length(Word, WordLength),
    debug(puzzle, 'Processing word: ~w (Length: ~w)', [Word, WordLength]),
    insert_word_in_dict(WordLength, Word, Acc, Acc2),
    group_words_by_length(RemainingWords, Acc2, Out).

/*********************************************************************
 * insert_word_in_dict/4
 * insert_word_in_dict(+WordLength, +Word, +DictIn, -DictOut)
 * Inserts a word into the appropriate length bucket in the dictionary
 *
 * Arguments:
 *   WordLength: Length of the word
 *   Word: The word to insert
 *   DictIn: Input dictionary
 *   DictOut: Dictionary after insertion
 *
 * Base Case: For a new word length, create a new length-bucket
 *      WordLength-[Word] in the dictionary.
 * Matching Length: If the word length matches an existing bucket,
 *      add the word to that bucket.
 * Different Length: If the word length does not match, recurse
 *********************************************************************/
insert_word_in_dict(WordLength, Word, [], [WordLength-[Word]]) :- 
    debug(puzzle, 'Creating new bucket for length ~w with word ~w', [WordLength, Word]),
    !.
insert_word_in_dict(WordLength, Word, [Len-Words|Rest], [Len-[Word|Words]|Rest]) :-
    WordLength == Len,
    debug(puzzle, 'Inserting word ~w into bucket of length ~w', [Word, WordLength]),    
    !.
insert_word_in_dict(WordLength, Word, [Pair|Rest], [Pair|NewRest]) :-
    insert_word_in_dict(WordLength, Word, Rest, NewRest).

/*********************************************************************
 * extract_all_slots/2
 * extract_all_slots(+Puzzle, -AllSlots)
 * Gathers horizontal & vertical runs (>=2) of open cells.
 *
 * Arguments:
 *   Puzzle: The crossword grid
 *   AllSlots: List of all slots found (horizontal and vertical)
 *
 * A slot is represented as slot(Orientation, Length, Coordinates)
 * where Orientation is 'horizontal' or 'vertical', Length is the
 * number of cells, and Coordinates is a list of (Row,Col) pairs
 * where the top-left corner of the grid as (0,0).
 *********************************************************************/
extract_all_slots(Puzzle, AllSlots) :-
    extract_horizontal_slots(Puzzle, HorizontalSlots),
    debug(puzzle, 'Horizontal Slots: ~w', [HorizontalSlots]),
    extract_vertical_slots(Puzzle, VerticalSlots),
    debug(puzzle, 'Vertical Slots: ~w', [VerticalSlots]),
    append(HorizontalSlots, VerticalSlots, AllSlots).

/*********************************************************************
 * extract_horizontal_slots/2
 * extract_horizontal_slots(+Puzzle, -HorizontalSlots)
 * Finds all horizontal slots in the puzzle
 *
 * Arguments:
 *   Puzzle: The crossword grid
 *   HorizontalSlots: List of horizontal slots found
 *
 * Run 'row_to_slots/3' for each row in the grid to find all
 * horizontal slots.
 *
 * SlotsPerRow is a list of lists, where each sublist contains
 * the slots found in that row.
 *
 * The final result is a flattened list of all horizontal slots, this
 * ensures that the output only contains slots and no empty lists.
 *********************************************************************/
extract_horizontal_slots(Puzzle, HorizontalSlots) :-
    length(Puzzle, NumRows),
    debug(puzzle, 'Number of rows in puzzle: ~w', [NumRows]),
    NumRowsMinus1 is NumRows - 1,

    findall(RowSlots,
      ( between(0, NumRowsMinus1, RowIndex),
        nth0(RowIndex, Puzzle, Row),
        debug(puzzle, 'Row index: ~w | Row: ~w', [RowIndex, Row]),
        row_to_slots(Row, RowIndex, RowSlots)
      ),
      SlotsPerRow),

    debug(puzzle, 'Slots per row: ~w', [SlotsPerRow]),
    flatten(SlotsPerRow, HorizontalSlots).

/*********************************************************************
 * row_to_slots/3
 * row_to_slots(+RowList, +RowIndex, -RowSlots)
 * Converts a row to a list of horizontal slots
 *
 * Arguments:
 *   RowList: List of cells in the row
 *   RowIndex: Index of the row in the puzzle
 *   RowSlots: List of slots found in this row
 *********************************************************************/
row_to_slots(RowList, RowIndex, RowSlots) :-
    collect_runs(RowList, 0, Runs),
    debug(puzzle, 'Runs found in row ~w: ~w', [RowIndex, Runs]),
    make_horizontal_slots(Runs, RowIndex, RowSlots).

/*********************************************************************
 * collect_runs/3
 * collect_runs(+List, +Col, -Runs)
 * Collects consecutive runs of non-blocked cells in a row
 *
 * Arguments:
 *   List: List of cells to process
 *   Col: Current column position
 *   Runs: List of runs found, each run is a list of column indices
 *
 * Base Case: After processing all cells, return empty list.
 * Blocked Cell: If a blocked cell is encountered, skip it and
 *      continue to the next cell.
 * Consecutive Cell: If an open cell is found, collect it and
 *      continue to collect consecutive cells until a blocked cell
 *      is encountered.
 *********************************************************************/
collect_runs([], _, []).
collect_runs([Cell|T], Col, Runs) :-
    Cell == '#',  % blocked => skip
    debug(puzzle, 'Blocked cell at column ~w', [Col]),
    NextCol is Col + 1,
    collect_runs(T, NextCol, Runs).

collect_runs([Cell|T], Col, [[Col|Cols] | OtherRuns]) :-
    Cell \== '#',  % open cell
    debug(puzzle, 'Open cell at column ~w', [Col]),
    ConsecutiveCol is Col + 1,
    collect_consecutive(T, ConsecutiveCol, Cols, Remainder),
    length(Cols, RunLength),
    % ConsecutiveCol is the starting column of the consecutive run
    % RunLength is the length of the consecutive run
    % + 1 for the next column after the consecutive run terminates
    NextCol is ConsecutiveCol + RunLength + 1,
    debug(puzzle, 'Next column to check: ~w', [NextCol]),
    collect_runs(Remainder, NextCol, OtherRuns).

/*********************************************************************
 * collect_consecutive/4
 * collect_consecutive(+List, +Col, -Cols, -Remainder)
 * Collects consecutive non-blocked cells
 *
 * Arguments:
 *   List: List of cells to process
 *   Col: Current column position
 *   Cols: Additional column indices in this run
 *   Remainder: Remaining cells after this run
 *
 * Base Case: After processing all cells, return empty list.
 * Blocked Cell: If a blocked cell is encountered, skip it and
 *      return the remaining cells.
 * Consecutive Cell: If an open cell is found, collect it and
 *      continue to collect consecutive cells until a blocked cell
 *********************************************************************/
collect_consecutive([], _Col, [], []).
collect_consecutive([Cell|T], Col, [], T) :-
    Cell == '#', 
    debug(puzzle, 'End of consecutive run at column ~w', [Col]),
    !.
collect_consecutive([Cell|T], Col, [Col|More], Remainder) :-
    Cell \== '#',
    debug(puzzle, 'Consecutive cell at column ~w', [Col]),
    NextCol is Col + 1,
    collect_consecutive(T, NextCol, More, Remainder).

/*********************************************************************
 * make_horizontal_slots/3
 * make_horizontal_slots(+RunCols, +Row, -Slots)
 * Converts runs of column indices to horizontal slots
 *
 * Arguments:
 *   RunCols: List of runs, each run is a list of column indices
 *   Row: Row index for this slot
 *   Slots: List of slots created
 *
 * Only creates slots for runs with length >= 2
 *
 * Base Case: If no runs left, return empty list.
 * Valid Run: If the run has length >= 2, create a slot
 *      with the coordinates of the cells in that run.
 * Invalid Run: If the run has length < 2, skip it.
 *********************************************************************/
make_horizontal_slots([], _, []).
make_horizontal_slots([RunCols|T], Row, [slot(horizontal,Len,Coords)|Slots]) :-
    length(RunCols, Len),
    Len >= 2,
    findall((Row,Col), member(Col, RunCols), Coords),
    make_horizontal_slots(T, Row, Slots).
make_horizontal_slots([RunCols|T], Row, Slots) :-
    length(RunCols, Len),
    Len < 2, % skip single-cell runs
    make_horizontal_slots(T, Row, Slots).

/*********************************************************************
 * extract_vertical_slots/2
 * extract_vertical_slots(+Puzzle, -VerticalSlots)
 * Finds all vertical slots in the puzzle by transposing the grid
 *
 * Arguments:
 *   Puzzle: The crossword grid
 *   VerticalSlots: List of vertical slots found
 *
 * Re-purposes the horizontal slot extraction code by transposing
 * the grid first. This allows us to treat vertical slots as
 * horizontal slots in the transposed grid.
 *
 * Finally fixes the coordinates back to the original grid by 
 * swapping the row and column indices.
 *********************************************************************/
extract_vertical_slots(Puzzle, VerticalSlots) :-
    transpose(Puzzle, TransposedGrid),
    extract_horizontal_slots(TransposedGrid, TempHSlots),
    fix_transposed_coords(TempHSlots, VerticalSlots).

/*********************************************************************
 * fix_transposed_coords/2
 * fix_transposed_coords(+TempHSlots, -VerticalSlots)
 * Converts horizontal slots in transposed grid to vertical slots
 *
 * Arguments:
 *   TempHSlots: Horizontal slots in transposed grid
 *   VerticalSlots: Resulting vertical slots in original grid
 *
 * TC represents the transposed coordinates of the slots (Col,Row)
 * OC represents the output coordinates of the slots (Row,Col)
 * 
 * Base Case: If no more slots left, return empty list.
 * Valid Slot: For each slot, swap the row and column indices
 * and the slot orientation.
 *********************************************************************/
fix_transposed_coords([], []).
fix_transposed_coords([slot(horizontal,Len,TC)|T],[slot(vertical,Len,OC)|T2]) :-
    maplist(swap_col_row_to_row_col, TC, OC),
    fix_transposed_coords(T, T2).

/*********************************************************************
 * swap_col_row_to_row_col/2
 * swap_col_row_to_row_col(+CR, -RC)
 * Swaps column-row coordinates to row-column coordinates
 *
 * Arguments:
 *   (C,R): Input column-row pair (transposed)
 *   (R,C): Output row-column pair (original grid)
 *********************************************************************/
swap_col_row_to_row_col((C,R),(R,C)).

/*********************************************************************
 * fill_puzzle/3
 * fill_puzzle(+Puzzle, +Slots, +WordDict)
 * Core solving algorithm that fills the puzzle with words
 *
 * Arguments:
 *   Puzzle: The crossword grid
 *   Slots: List of slots to fill
 *   WordDict: Dictionary of available words
 *
 * Strategy:
 * - If no more slots => check dictionary is empty => success or fail.
 * - Else pick the "most constrained slot" => minimal # of candidate words.
 * - For each candidate word:
 *     - place it, remove from dict, recurse. 
 *     - on failure => backtrack.
 *********************************************************************/
fill_puzzle(_Puzzle, [], WordDict) :-
    (   WordDict == [] 
    ->  debug(puzzle, 'All slots filled successfully!', []),
        !
    ;   debug(puzzle, 'Leftover words in dictionary: ~w', [WordDict]),
        fail
    ).

fill_puzzle(Puzzle, Slots, WordDict) :-
    pick_most_constrained_slot(Puzzle, Slots, WordDict, Slot, OtherSlots),
    try_fill_slot_with_candidates(Puzzle, Slot, OtherSlots, WordDict).

/*********************************************************************
 * pick_most_constrained_slot/5
 * pick_most_constrained_slot(+Puzzle, +Slots, +WordDict, -SelectedSlot, -OtherSlots)
 * Evaluate candidate count for each slot => pick the one with min #.
 *
 * Arguments:
 *   Puzzle: The crossword grid
 *   Slots: List of slots to evaluate
 *   WordDict: Dictionary of available words
 *   SelectedSlot: Selected slot with fewest candidates
 *   OtherSlots: Remaining slots (excluding the selected one)
 *
 * Strategy:
 * - First calculate the number of candidates for the first slot.
 * - Recursively check the rest of the slots with the helper predicate.
 * - Return the selected slot and the remaining slots.
 * - Remove that selected slot from the list of slots (as it is now in-use).
 *********************************************************************/
pick_most_constrained_slot(Puzzle, [Slot|Slots], WordDict, SelectedSlot, OtherSlots) :-
    debug(puzzle, 'Evaluating slot: ~w', [Slot]),
    candidates_for_slot(Puzzle, Slot, WordDict, Candidates),
    length(Candidates, NumberOfCandidates),
    Slot = slot(_, SlotLength, _),
    pick_most_constrained_slot_aux(Puzzle, Slots, WordDict, Slot, NumberOfCandidates, SlotLength, SelectedSlot),
    delete([Slot|Slots], SelectedSlot, OtherSlots).

/*********************************************************************
 * pick_most_constrained_slot_aux/6
 * pick_most_constrained_slot_aux(+Puzzle, +Slots, +WordDict, +BestSlotSoFar,
 *                                +LowestCount, -SelectedSlot)
 * Helper to find the slot with the fewest candidate words
 *
 * Arguments:
 *   Puzzle: The crossword grid
 *   Slots: Remaining slots to check
 *   WordDict: Dictionary of available words
 *   BestSlotSoFar: Best slot found so far
 *   LowestCount: Number of candidates for best slot so far (lower is better)
 *   SelectedSlot: Selected slot with the fewest candidates
 *
 * Base Case: After recursing through all slots, return the best slot
 *      BestSlotSoFar is returned as the final slot.
 * Valid Case: If the current slot has fewer candidates than the best
 *      slot so far, update the best slot and continue searching.
 *********************************************************************/
pick_most_constrained_slot_aux(_Puzzle, [], _WDict, Slot, _Count, _Length, Slot).
pick_most_constrained_slot_aux(Puzzle, [CurrentSlot|Slots], WordDict, BestSlotSoFar, LowestCount, HighestSlotLength, SelectedSlot) :-
    debug(puzzle, 'Evaluating in aux slot: ~w', [CurrentSlot]),
    candidates_for_slot(Puzzle, CurrentSlot, WordDict, CurrentCandidates),
    length(CurrentCandidates, CurrentCount),
    CurrentSlot = slot(_, CurrentSlotLength, _),
    ( CurrentCount < LowestCount ->
        debug(puzzle, 'Found better slot: ~w with ~w candidates', [CurrentSlot, CurrentCount]),
        pick_most_constrained_slot_aux(Puzzle, Slots, WordDict, CurrentSlot, CurrentCount, CurrentSlotLength, SelectedSlot)
    ; CurrentCount == LowestCount, CurrentSlotLength > HighestSlotLength ->
        debug(puzzle, 'Found equally constrained slot: ~w with ~w candidates', [CurrentSlot, CurrentCount]),
        pick_most_constrained_slot_aux(Puzzle, Slots, WordDict, CurrentSlot, LowestCount, CurrentSlotLength, SelectedSlot)
    ; pick_most_constrained_slot_aux(Puzzle, Slots, WordDict, BestSlotSoFar, LowestCount, HighestSlotLength, SelectedSlot)
    ).

/*********************************************************************
 * try_fill_slot_with_candidates/4
 * try_fill_slot_with_candidates(+Puzzle, +Slot, +OtherSlots, +WordDict)
 * Attempt each candidate word in turn. If all fail => backtrack.
 *
 * Arguments:
 *   Puzzle: The crossword grid
 *   Slot: The slot to fill
 *   OtherSlots: Remaining slots to fill
 *   WordDict: Dictionary of available words
 *
 * Strategy:
 *   - Get all candidates for this slot.
 *   - If no candidates => fail.
 *   - For each candidate:
 *       - Place the word in the slot.
 *       - Remove the word from the dictionary.
 *       - Recurse to fill the rest of the slots.
 *       - If that fails, backtrack and try the next candidate.
 *   - If all candidates fail, backtrack to the previous slot.
 *   - If all slots are filled successfully, return the filled puzzle.
 *********************************************************************/
try_fill_slot_with_candidates(Puzzle, Slot, OtherSlots, WordDict) :-
    candidates_for_slot(Puzzle, Slot, WordDict, Candidates),
    ( Candidates = [] ->
        debug(puzzle, 'No candidates for slot ~w => fail', [Slot]),
        fail
    ; true ),
    % member is non-deterministic => backtrack on failure. 
    member(Word, Candidates),

    debug(puzzle, 'Trying to place word ~w in slot ~w', [Word, Slot]),
    place_word_in_slot(Puzzle, Slot, Word),

    % Retrieve the length of the slot that the word was placed in
    Slot = slot(_, WordLength, _),
    remove_word_from_dict(WordDict, WordLength, Word, WordDict2),
    fill_puzzle(Puzzle, OtherSlots, WordDict2).

/*********************************************************************
 * candidates_for_slot/4
 * candidates_for_slot(+Puzzle, +Slot, +WordDict, -Candidates)
 * Returns all valid candidate words from WordDict that:
 *   A: match the slot's length,
 *   B: don't conflict with puzzle's partially filled letters.
 *
 * Arguments:
 *   Puzzle: The crossword grid
 *   slot(_Orient, Len, Coords): The slot to find candidates for
 *   WordDict: Dictionary of available words
 *   Candidates: List of valid candidates for this slot
 *
 * Strategy:
 *   - Get all words of the same length from the dictionary.
 *   - Filter the candidates to only include those that
 *     are valid for the given slot.
 *
 * Note: Orientation is only used for debugging and not actually
 *       relevant to the candidate selection.
 *********************************************************************/
candidates_for_slot(Puzzle, slot(Orient, Len, Coords), WordDict, Candidates) :-
    get_words_of_length(WordDict, Len, WordCandidates),
    debug(puzzle, 'Word candidates for length ~w: ~w', [Len, WordCandidates]),

    include(validate_candidate(Puzzle, Coords), WordCandidates, Candidates),
    debug(puzzle, 'Valid candidates for slot ~w: ~w', [slot(Orient, Len, Coords), Candidates]).

/*********************************************************************
 * get_words_of_length/3
 * get_words_of_length(+WordDict, +WordLength, -WordCandidates)
 * Retrieve the list of words for 'WordLength' from the dictionary.
 *
 * Arguments:
 *   WordDict: The word dictionary
 *   WordLength: Length of the words to retrieve
 *   WordCandidates: List of words of length 'WordLength'
 *
 * Base Case: After processing all words for a given length, return empty list.
 * Valid Case: If the length matches, return the list of words.
 * Invalid Case: If the length does not match, continue searching.
 *********************************************************************/
get_words_of_length([], _L, []).
get_words_of_length([Len-WordCandidates|_], WordLength, WordCandidates) :- WordLength == Len, !.
get_words_of_length([_|Rest], WordLength, WordCandidates) :-
    get_words_of_length(Rest, WordLength, WordCandidates).

/*********************************************************************
 * validate_candidate/3
 * validate_candidate(+Puzzle, +Coords, +Word)
 * Checks if a word is valid for a given slot
 *
 * Arguments:
 *   Puzzle: The crossword grid
 *   Coords: List of (Row,Col) coordinates for the slot
 *   Word: The word to validate
 *********************************************************************/
validate_candidate(Puzzle, Coords, Word) :-
    validate_cells(Puzzle, Coords, Word).

/*********************************************************************
 * validate_cells/3
 * validate_cells(+Puzzle, +Coords, +Word)
 * Checks if a word is compatible with existing letters in the grid
 *
 * Arguments:
 *   Puzzle: The crossword grid
 *   Coords: List of (Row,Col) coordinates to check
 *   Word: List of letters to validate
 *
 * Base Case: If no more cells or letters to check, return empty list.
 * Valid Case: If the cell is unbound or already matches the letter,
 *      continue checking the next cell and letter.
 *********************************************************************/
validate_cells(_Puzzle, [], []).
validate_cells(Puzzle, [(RowIndex, ColIndex)|Coords], [Letter|Rest]) :-
    % Retrieve Row based on RowIndex
    nth0(RowIndex, Puzzle, Row),
    % Retrieve Cell based on Row and ColIndex
    nth0(ColIndex, Row, Cell),
    % Check if Cell is unbound or already matches Letter, but do not unify
    (   var(Cell) -> true
    ;   Cell == Letter -> true
    ),
    validate_cells(Puzzle, Coords, Rest). 

/*********************************************************************
 * place_word_in_slot/3
 * place_word_in_slot(+Puzzle, +Slot, +Word)
 * Unifies puzzle cells with Word letters
 *
 * Arguments:
 *   Puzzle: The crossword grid
 *   slot(Orient,Len,Coords): The slot to fill
 *   Word: The word to place in the slot
 *
 * Note: Orientation and Length are not used in this predicate.
 *
 * Base Case: If the slot has no more coordinates, 
 *      the word has been placed successfully.
 * Valid Case: If the slot has coordinates, place the letter in the
 *      corresponding cell and recurse for the next letter.
 *
 * No need to revert if we fail because Prolog automatically backtracks
 * these unifications.
 *********************************************************************/
place_word_in_slot(_Puzzle, slot(_Orient, _Len, []), []).
place_word_in_slot(Puzzle, slot(Orient, Len, [(RowIndex,ColIndex)|Coords]), [Letter|Rest]) :-
    % Retrieve Row based on RowIndex
    nth0(RowIndex, Puzzle, Row),
    % Retrieve Cell based on Row and ColIndex
    nth0(ColIndex, Row, Cell),
    % Ensure Cell is unbound or already matches Letter and unify
    ( var(Cell) ->
        Cell = Letter
    ; true  % If it's already that letter => okay, else fail in conflict check. 
    ),
    place_word_in_slot(Puzzle, slot(Orient, Len, Coords), Rest).

/*********************************************************************
 * remove_word_from_dict/4
 * remove_word_from_dict(+DictIn, +WordLength, +Word, -DictOut)
 * Remove 'Word' from the dictionary bucket for 'Length'.
 *
 * Arguments:
 *   DictIn: Input dictionary
 *   WordLength: Length of the word
 *   Word: The word to remove
 *   DictOut: Dictionary after removal
 *
 * If that bucket becomes empty afterwards, remove that pair entirely.
 *
 * Base Case: Failsafe, as the dictionary should always contain the word
 *      being removed.
 * Valid Case: If the length matches, remove the word from that bucket
 *      and return an updated dictionary.
 * Valid Case: If the length matches and there is only one word left
 *      in that bucket, remove the entire bucket.
 * Invalid Case: If the length does not match, continue searching.
 *********************************************************************/
% Base Case; Empty dictionary => fail
remove_word_from_dict([], _WordLength, _Word, []) :- 
    fail.

% Valid Case; WordLength matches, but there are still words left
remove_word_from_dict([Len-Words|Rest], WordLength, Word, [Len-NewWords|Rest]) :-
    WordLength == Len,
    select(Word, Words, NewWords),
    NewWords \= [],
    !.

% Valid Case; WordLength matches, but this is the last word in the bucket
remove_word_from_dict([Len-[Word]|Rest], WordLength, Word, Rest) :-
    WordLength == Len, !.

% Invalid Case; WordLength does not match, continue searching
remove_word_from_dict([Pair|Rest], WordLength, W, [Pair|NewRest]) :-
    remove_word_from_dict(Rest, WordLength, W, NewRest).

/*********************************************************************
 * print_puzzle/1
 * print_puzzle(+Puzzle)
 * Prints the completed puzzle to the console
 *
 * Arguments:
 *   Puzzle: The crossword grid to print
 *********************************************************************/
print_puzzle(Puzzle) :-
    debug(puzzle, 'Completed Puzzle:', []),
    maplist(print_row, Puzzle).

print_row(Row) :-
    debug(puzzle, '~w', [Row]).

/*********************************************************************
 * Tests created using SWI-Prolog's plunit library
 * To run a specific test, use:
    ?- run_tests(puzzle_solution:example).
 * To run all tests, use:
    ?- run_tests(puzzle_solution).

 * Test cases are written to expect at most one solution.

 * Debugging is forcibly enabled by default for all tests
 * via set_test_options([output(always)]) and setup(debug(puzzle)).
 *********************************************************************/
:- begin_tests(puzzle_solution, [setup(debug(puzzle))]).
:- set_test_options([output(always)]).

/* Test Case 1: example */  
test(example, true(Solutions = [ExpectedPuzzle])) :-
    debug(puzzle, '>>> STARTING EXAMPLE TEST <<<', []),
    Puzzle = [
        ['#', h, '#'],
        [ _,  _,  _ ],
        ['#', _, '#']
    ],
    WordList = [
        [h, a, t],
        [b, a, g]
    ],
    copy_term(Puzzle, Sol),
    findall(Sol, puzzle_solution(Sol, WordList), Solutions),
    maplist(print_puzzle, Solutions),
    ExpectedPuzzle = [
        ['#', h, '#'],
        [ b,  a,  g ],
        ['#', t, '#']
    ],
    debug(puzzle, '>>> EXAMPLE TEST PASSED <<<', []).

/* Test Case 2: small_puzzle */
test(small_puzzle, true(Solutions = [ExpectedPuzzle])) :-
    debug(puzzle, '>>> STARTING SMALL PUZZLE TEST <<<', []),
    Puzzle = [
        [ _,  '#', _ ],
        [ _,   _,  _ ],
        [ '#', _, '#' ]
    ],
    WordList = [
        [e, c],
        [i, t],
        [c, a, t],
        [a, r]
    ],
    copy_term(Puzzle, Sol),
    findall(Sol, puzzle_solution(Sol, WordList), Solutions),
    maplist(print_puzzle, Solutions),
    ExpectedPuzzle = [
        [ e, '#', i ],
        [ c,  a,  t ],
        ['#', r, '#' ]
    ],
    debug(puzzle, '>>> SMALL PUZZLE TEST PASSED <<<', []).

/* Test Case 3: wikipedia puzzle */
test(wikipedia, true(Solutions = [ExpectedPuzzle])) :-
    debug(puzzle, '>>> STARTING WIKIPEDIA PUZZLE TEST <<<', []),
    Puzzle = [
        ['#', '#', '#',  _ ,  _ ,  _ , '#', '#', '#', '#'],
        ['#', '#', '#',  _ ,  _ ,  _ ,  _ , '#', '#', '#'],
        ['#',  d ,  _ ,  _ ,  _ , '#',  _ ,  _ , '#', '#'],
        ['#',  a ,  _ ,  _ ,  _ ,  _ ,  _ ,  _ , '#', '#'],
        ['#',  g ,  _ , '#',  _ ,  _ ,  _ ,  _ , '#', '#'],
        ['#', '#',  _ ,  _ ,  _ ,  _ , '#', '#', '#', '#'],
        ['#', '#', '#',  _ ,  _ ,  _ , '#', '#', '#', '#']
    ],
    WordList = [
        [g, i],
        [i, o],
        [o, n],
        [o, r],
        [d, a, g],
        [e, v, o],
        [o, e, d],
        [r, e, f],
        [a, r, i, d],
        [c, l, e, f],
        [c, l, o, d],
        [d, a, i, s],
        [d, e, n, s],
        [d, o, l, e],
        [e, d, i, t],
        [s, i, l, o],
        [a, r, t, i, c, l, e],
        [v, e, s, i, c, l, e]
    ],
    copy_term(Puzzle, Sol),
    findall(Sol, puzzle_solution(Sol, WordList), Solutions),
    maplist(print_puzzle, Solutions),
    ExpectedPuzzle = [
        ['#', '#', '#',  e ,  v ,  o , '#', '#', '#', '#'],
        ['#', '#', '#',  d ,  e ,  n ,  s , '#', '#', '#'],
        ['#',  d ,  a ,  i ,  s , '#',  i ,  o , '#', '#'],
        ['#',  a ,  r ,  t ,  i ,  c ,  l ,  e , '#', '#'],
        ['#',  g ,  i , '#',  c ,  l ,  o ,  d , '#', '#'],
        ['#', '#',  d ,  o ,  l ,  e , '#', '#', '#', '#'],
        ['#', '#', '#',  r ,  e ,  f , '#', '#', '#', '#']
    ],
    debug(puzzle, '>>> WIKIPEDIA PUZZLE TEST PASSED <<<', []).

/* Test Case 4: More slots than words (should fail) */
test(more_slots_than_words, true(Solutions = [])) :-
    debug(puzzle, '>>> STARTING MORE SLOTS THAN WORDS TEST <<<', []),
    Puzzle = [
        [ _,  _,  _,  _ ],
        [ _,  _,  _,  _ ],
        [ _,  _,  _,  _ ],
        [ _,  _,  _,  _ ]
    ],
    WordList = [
        [c, a, t],
        [d, o, g]
    ],
    copy_term(Puzzle, Sol),
    findall(Sol, puzzle_solution(Sol, WordList), Solutions),
    debug(puzzle, '>>> MORE SLOTS THAN WORDS TEST FAILED <<<', []).

/* Test Case 5: More words than slots (should fail) */
test(more_words_than_slots, true(Solutions = [])) :-
    debug(puzzle, '>>> STARTING MORE WORDS THAN SLOTS TEST <<<', []),
    Puzzle = [
        ['#', _, '#'],
        ['#', _, '#'],
        ['#', _, '#']
    ],
    WordList = [
        [a, b, c],
        [d, e, f]
    ],
    copy_term(Puzzle, Sol),
    findall(Sol, puzzle_solution(Sol, WordList), Solutions),
    debug(puzzle, '>>> MORE WORDS THAN SLOTS TEST FAILED <<<', []).

/* Test Case 6: Most constrained slot challenge */
test(most_constrained_slot, true(Solutions = [ExpectedPuzzle])) :-
    debug(puzzle, '>>> STARTING MOST CONSTRAINED SLOT TEST <<<', []),
    Puzzle = [
        [ _,  _,  r,  _ ],
        [ _,  _,  a,  _ ],
        [ _,  _,  _,  _ ],
        [ _,  _,  _,  _ ]
    ],
    WordList = [
        [c, a, r, t],
        [z, x, a, r],
        [h, a, t, e],
        [c, k, e, e],
        [c, z, h, c],
        [a, x, a, k],
        [r, a, t, e],
        [t, r, e, e]
    ],
    copy_term(Puzzle, Sol),
    findall(Sol, puzzle_solution(Sol, WordList), Solutions),
    maplist(print_puzzle, Solutions),
    ExpectedPuzzle = [
        [ c,  a,  r,  t ],
        [ z,  x,  a,  r ],
        [ h,  a,  t,  e ],
        [ c,  k,  e,  e ]
    ],
    debug(puzzle, '>>> MOST CONSTRAINED SLOT TEST PASSED <<<', []).

/* Test Case 7: 10x10 sized puzzle */
test(ten_by_ten, true(Solutions = [ExpectedPuzzle])) :-
    debug(puzzle, '>>> STARTING 10x10 PUZZLE TEST <<<', []),
    Puzzle = [
        [ _,  _,  _,  _,  _,  _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _,  _, '#', _,  _,  _,  _ ],
        [ _,  _, '#', _,  _, '#', _,  _,  _,  _ ],
        [ _,  _, '#', _,  _,  _,  _, '#', _,  _ ],
        [ _,  _,  _,  _,  _,  _,  _,  _,  _,  _ ],
        [ _,  _, '#', _,  _, '#', _,  _,  _,  _ ],
        [ _,  _,  _,  _,  _,  _,  _, '#', _,  _ ],
        [ _,  _,  _, '#', _,  _,  _,  _,  _, '#'],
        [ _,  _,  _,  _,  _,  _,  _,  _,  _,  _ ],
        ['#', _,  _, '#', _,  _, '#', _,  _,  _ ]
    ],
    WordList = [
        % Horitzontal
        [b, l, a, c, k, s, h, i, r, t],
        [e, a, g, l, e],
        [a, g, e, d],
        [a, m],
        [e, z],
        [i, s, h, m],
        [u, e],
        [a, i, p, r],
        [h, r],
        [t, r, n, n, o, q, s, o, c, d],
        [i, o],
        [e, a],
        [t, l, e, m],
        [f, p, q, d, f, s, y],
        [p, o],
        [u, m, r],
        [a, t, l, s, h],
        [l, w, n, e, i, u, e, y, d, x],
        [m, e],
        [r, v],
        [t, u, v],
        % Vertical
        [b, e, a, u, t, i, f, u, l],
        [l, a, m, e, r, o, p, m, w, m],
        [a, g],
        [q, r, n, e],
        [c, l, e, a, n, e, d],
        [k, e, z, i, o, a, f, a, i, r],
        [p, q],
        [s, t, u, v],
        [h, a, i, r, s, t, y, l, e],
        [i, g, s],
        [o, l],
        [s, y, t],
        [r, e, h, h, c, e, p, h, d, u],
        [t, d, m, r, d, m, o],
        [x, v]
    ],
    copy_term(Puzzle, Sol),
    findall(Sol, puzzle_solution(Sol, WordList), Solutions),
    maplist(print_puzzle, Solutions),
    ExpectedPuzzle = [
        [ b,  l,  a,  c,  k,  s,  h,  i,  r,  t ],
        [ e,  a,  g,  l,  e, '#', a,  g,  e,  d ],
        [ a,  m, '#', e,  z, '#', i,  s,  h,  m ],
        [ u,  e, '#', a,  i,  p,  r, '#', h,  r ],
        [ t,  r,  n,  n,  o,  q,  s,  o,  c,  d ],
        [ i,  o, '#', e,  a, '#', t,  l,  e,  m ],
        [ f,  p,  q,  d,  f,  s,  y, '#', p,  o ],
        [ u,  m,  r, '#', a,  t,  l,  s,  h, '#'],
        [ l,  w,  n,  e,  i,  u,  e,  y,  d,  x ],
        ['#', m,  e, '#', r,  v, '#', t,  u,  v ]
    ],
    debug(puzzle, '>>> 10x10 PUZZLE TEST PASSED <<<', []).

/* Test Case 8: 15x15 sized puzzle */
test(fifteen_by_fifteen, true(Solutions = [ExpectedPuzzle])) :-
    debug(puzzle, '>>> STARTING 15x15 PUZZLE TEST <<<', []),
    Puzzle = [
        [ _, '#', _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _ ],
        [ _,  _,  _, '#', _,  _,  _, '#', _,  _,  _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _, '#', _,  _,  _,  _,  _, '#',  _,  _,  _,  _ ],
        [ _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _, '#', _,  _ ],
        [ _,  _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _, '#'],
        [ _,  _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _ ],
        [ _,  _, '#', _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _, '#', _,  _ ],
        [ _,  _,  _, '#', _,  _, '#', _,  _, '#', _,  _,  _,  _,  _ ],
        ['#', _,  _,  _,  _,  _,  _,  _,  _,  _,  _,  _, '#', _,  _ ],
        [ _,  _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _ ],
        [ _,  _,  _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _ ]
    ],
    WordList = [
        % Horitzontal
        [s, l, e, e, p, y],
        [g, i, r, l, i, e],
        [b, o, b],
        [t, h, e],
        [b, u, i, l, d, e, r],
        [s, k, e, l, e, t, o, n],
        [s, h, i, v, e, r],
        [o, p, a, l],
        [b, a, k, e, h],
        [t, l, d, r],
        [l, i, v, i, n, g],
        [s, l, i, m, e],
        [a, h],
        [u, m, b, r, e, l, l, a],
        [n, o, s, e, y],
        [t, e, l, e, p, o, r, t],
        [g, r, a, v, e, l],
        [e, h],
        [b, u, b, b, l, e],
        [s, h, o, r, t],
        [l, o, v, e, l, y],
        [b, r, a, i, n],
        [o, h],
        [y, e, s],
        [o, r],
        [n, o],
        [c, l, a, i, m],
        [e, x, c, l, a, m, a, t, i, o, n],
        [g, o],
        [h, e, a, t, h, e, n, s],
        [l, a, p, t, o, p],
        [a, c, t, u, a, l, l, y],
        [h, u, n, g, r, y],
        [r, a, n, d, o, m, i, s, e, d],
        [b, a, b, e],
        [d, e, l, i, r, i, o, u, s],
        [p, l, a, t, e],
        % Vertical
        [a, b, s, o, l, u, t, e, l, y],
        [h, a, r, d],
        [o, k, p, i, m, e, h, o, e, e, e, c, a, e],
        [s, b, e, a, v, b, l],
        [v, s, x, a, t, n, l],
        [l, l, i, r, e, b, e],
        [c, t, u, d, i],
        [e, t, e],
        [n, e, p, u, l, o, l, h, a, o, r],
        [e, h, t, b, g, l, o, b, y, r, a, e, l, m, i],
        [p, e, o, a],
        [l, r, b],
        [m, n, l, i, o],
        [n, k, s, a, t, l, b, n, a, s, y, s, u],
        [e, l],
        [e, r, o, t],
        [e, s],
        [g, u, s, h, i, n, g],
        [i, l, h, d],
        [i, i ,h],
        [m, o, r, s, i, c, o, a, u],
        [r, l, i, t, e, s, a, h, n, l, n, p, n, b, l],
        [l, d, v, l],
        [e, v, o],
        [t, g, a, a],
        [i, e, e, d, a, y, e, r, o, i, g, o, r, b, t],
        [e, r, r, r, h],
        [l, t, h, m, o, p, y, e, e]
    ],
    copy_term(Puzzle, Sol),
    findall(Sol, puzzle_solution(Sol, WordList), Solutions),
    maplist(print_puzzle, Solutions),
    ExpectedPuzzle = [
        [ a, '#', s,  l,  e,  e,  p,  y, '#', g,  i,  r,  l,  i,  e ],
        [ b,  o,  b, '#', t,  h,  e, '#', b,  u,  i,  l,  d,  e,  r ],
        [ s,  k,  e,  l,  e,  t,  o,  n, '#', s,  h,  i,  v,  e,  r ],
        [ o,  p,  a,  l, '#', b,  a,  k,  e,  h, '#', t,  l,  d,  r ],
        [ l,  i,  v,  i,  n,  g, '#', s,  l,  i,  m,  e, '#', a,  h ],
        [ u,  m,  b,  r,  e,  l,  l,  a, '#', n,  o,  s,  e,  y, '#'],
        [ t,  e,  l,  e,  p,  o,  r,  t, '#', g,  r,  a,  v,  e,  l ],
        [ e,  h, '#', b,  u,  b,  b,  l,  e, '#', s,  h,  o,  r,  t ],
        [ l,  o,  v,  e,  l,  y, '#', b,  r,  a,  i,  n, '#', o,  h ],
        [ y,  e,  s, '#', o,  r, '#', n,  o, '#', c,  l,  a,  i,  m ],
        ['#', e,  x,  c,  l,  a,  m,  a,  t,  i,  o,  n, '#', g,  o ],
        [ h,  e,  a,  t,  h,  e,  n,  s, '#', l,  a,  p,  t,  o,  p ],
        [ a,  c,  t,  u,  a,  l,  l,  y, '#', h,  u,  n,  g,  r,  y ],
        [ r,  a,  n,  d,  o,  m,  i,  s,  e,  d, '#', b,  a,  b,  e ],
        [ d,  e,  l,  i,  r,  i,  o,  u,  s, '#', p,  l,  a,  t,  e ]
    ],
    debug(puzzle, '>>> 15x15 PUZZLE TEST PASSED <<<', []).

/* Test Case 9: 15x15 #2 */
test(fifteen_by_fifteen_2, true(Solutions = [ExpectedPuzzle])) :-
    debug(puzzle, '>>> STARTING 15x15 #2 PUZZLE TEST <<<', []),
    Puzzle = [
        [ _,  _,  _,  _, '#', _,  _,  _,  _, '#', _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _, '#', _,  _,  _,  _, '#', _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _, '#', _,  _,  _,  _, '#', _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _, '#','#','#'],
        ['#','#','#', _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _,  _, '#', _,  _,  _,  _, '#', _,  _,  _,  _ ],
        [ _,  _,  _, '#','#', _,  _,  _,  _,  _, '#','#', _,  _,  _ ],
        [ _,  _,  _,  _, '#', _,  _,  _,  _, '#', _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _, '#','#','#'],
        ['#','#','#', _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _,  _, '#', _,  _,  _,  _, '#', _,  _,  _,  _ ],
        [ _,  _,  _,  _,  _, '#', _,  _,  _,  _, '#', _,  _,  _,  _ ],
        [ _,  _,  _,  _,  _, '#', _,  _,  _,  _, '#', _,  _,  _,  _ ]
    ],
    WordList = [
        [a,r,i], [l,c,d], [n,s,a], [s,u,r],
        [a,c,i,s], [a,c,t,i], [a,i,e,e], [a,l,t,a],
        [a,r,a,n], [a,r,c,o], [a,s,i,c], [b,e,d,s],
        [b,i,n,o], [b,o,a,g], [c,a,a,n], [d,a,i,l],
        [e,a,l,e], [e,a,s,t], [e,l,s,a], [e,t,t,a],
        [h,e,r,n], [h,u,a,c], [i,b,i,s], [i,g,l,u],
        [i,i,r,c], [i,n,l,y], [l,a,s,t], [l,o,w,n],
        [m,i,c,a], [p,f,u,i], [r,a,y,a], [r,c,m,p],
        [t,a,f,e], [t,a,o,s], [u,n,c,e], [u,r,b,s],
        [y,a,r,t], [z,o,o,b], [j,o,s,e,p,h],
        [a,b,o,d,e], [a,l,i,f,e], [a,n,u,a,l], [a,u,l,o,s],
        [c,a,j,o,n], [c,a,n,a,l], [c,o,b,i,a], [e,a,s,e,r],
        [e,n,a,c,t], [g,i,g,o,t], [h,e,l,o,t], [k,a,r,r,i],
        [k,i,s,a,n], [o,a,s,i,s], [o,n,i,o,n], [o,r,a,c,y],
        [r,a,b,b,i], [r,a,i,d,s], [r,u,t,i,n], [s,c,a,d,a],
        [t,s,u,b,a], [t,w,i,x,t], [y,d,r,a,d],
        [a,n,c,h,o,r], [a,s,p,e,c,t], [e,a,t,e,r,y],
        [e,m,b,r,y,o], [o,d,e,s,s,a],
        [r,o,c,o,c,o], [s,m,i,g,h,t],
        [a,n,a,l,y,s,t], [a,p,p,l,i,e,s], [e,c,o,l,o,g,y],
        [f,i,x,t,u,r,e], [i,n,s,t,e,a,d], [o,e,r,s,t,e,d],
        [t,h,e,r,e,i,n], [t,h,y,r,o,i,d], [u,t,i,l,i,z,e]
    ],
    copy_term(Puzzle, Sol),
    findall(Sol, puzzle_solution(Sol, WordList), Solutions),
    maplist(print_puzzle, Solutions),
    ExpectedPuzzle = [
        [ a,  c,  t,  i, '#', e,  a,  l,  e, '#', r,  a,  b,  b,  i ],
        [ l,  o,  w,  n, '#', m,  i,  c,  a, '#', o,  n,  i,  o,  n ],
        [ i,  b,  i,  s, '#', b,  e,  d,  s, '#', c,  a,  n,  a,  l ],
        [ f,  i,  x,  t,  u,  r,  e, '#', e,  c,  o,  l,  o,  g,  y ],
        [ e,  a,  t,  e,  r,  y, '#', o,  r,  a,  c,  y, '#','#','#'],
        ['#','#','#', a,  b,  o,  d,  e, '#', j,  o,  s,  e,  p,  h ],
        [ r,  a,  i,  d,  s, '#', a,  r,  c,  o, '#', t,  a,  f,  e ],
        [ a,  r,  i, '#','#', k,  i,  s,  a,  n, '#','#', s,  u,  r ],
        [ y,  a,  r,  t, '#', a,  l,  t,  a, '#', r,  u,  t,  i,  n ],
        [ a,  n,  c,  h,  o,  r, '#', e,  n,  a,  c,  t, '#','#','#'],
        ['#','#','#', y,  d,  r,  a,  d, '#', s,  m,  i,  g,  h,  t ],
        [ t,  h,  e,  r,  e,  i,  n, '#', a,  p,  p,  l,  i,  e,  s ],
        [ a,  u,  l,  o,  s, '#', u,  n,  c,  e, '#', i,  g,  l,  u ],
        [ o,  a,  s,  i,  s, '#', a,  s,  i,  c, '#', z,  o,  o,  b ],
        [ s,  c,  a,  d,  a, '#', l,  a,  s,  t, '#', e,  t,  t,  a ]
    ],
    debug(puzzle, '>>> 15x15 #2 PUZZLE TEST PASSED <<<', []).

test(thirty_two_by_twenty, true(Solutions == [ExpectedPuzzle])) :-
    debug(puzzle, '>>> STARTING 32x20 PUZZLE TEST <<<', []),
    Puzzle = [
        [ _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _, '#', _ ],
        [ _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _, '#', _,  _, '#', _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _, '#', _,  _,  _,  _, '#', _,  _,  _,  _,  _, '#', _,  _,  _,  _ ],
        [ _,  _,  _, '#', _,  _,  _,  _, '#', _,  _, '#', _,  _,  _,  _,  _,  _, '#', _ ],
        [ _,  _, '#', _,  _,  _,  _,  _,  _,  _, '#', _,  _, '#', _,  _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _, '#', _,  _,  _, '#', _,  _,  _,  _,  _,  _, '#', _,  _,  _, '#'],
        [ _,  _,  _, '#', _,  _,  _, '#', _,  _,  _,  _, '#', _,  _,  _,  _, '#', _,  _ ],
        [ _, '#', _,  _,  _,  _, '#', _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _,  _ ],
        [ _, '#', _,  _, '#', _,  _,  _,  _,  _,  _, '#', _,  _,  _, '#', _,  _,  _,  _ ],
        ['#', _,  _,  _,  _, '#', _,  _, '#', _,  _,  _, '#', _,  _,  _,  _,  _, '#', _ ],
        [ _,  _,  _,  _,  _, '#', _,  _,  _, '#', _,  _,  _,  _,  _,  _, '#', _,  _,  _ ],
        [ _,  _,  _, '#', _,  _,  _,  _, '#', _,  _,  _, '#', _,  _,  _,  _,  _,  _,  _ ],
        [ _,  _, '#', _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _ ],
        [ _,  _,  _, '#', _,  _,  _, '#', _,  _,  _,  _,  _,  _,  _,  _,  _, '#', _,  _ ],
        [ _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _, '#'],
        [ _,  _, '#', _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _, '#', _,  _,  _ ],
        [ _,  _,  _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _,  _, '#', _,  _ ],
        [ _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _ ],
        [ _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _, '#', _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _, '#', _,  _ ],
        [ _,  _,  _, '#', _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _, '#', _,  _,  _ ],
        ['#', _,  _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _, '#', _,  _,  _ ],
        [ _,  _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _,  _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _,  _,  _,  _,  _,  _,  _,  _,  _,  _,  _,  _,  _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _,  _,  _,  _, '#','#'],
        [ _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _ ],
        [ _,  _,  _, '#', _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _ ],
        ['#', _,  _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _,  _,  _, '#','#'],
        [ _,  _,  _,  _,  _,  _,  _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _ ],
        [ _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _ ],
        [ _,  _,  _, '#', _,  _,  _,  _,  _,  _,  _,  _,  _,  _, '#', _,  _,  _,  _,  _ ]
    ],
    WordList = [
        % Horizontal
        [t, e, e, t, h],
        [a, m, a, z, i, n, g],
        [h, i, g, h],
        [e, s, m, o, i],
        [g, r, e, a, t],
        [a, m],
        [z, e, u, u, r],
        [r, t, o, p],
        [c, a, r, d],
        [u, m, m, m, m],
        [k, n, e, e],
        [r, a, t],
        [h, e, l, p],
        [m, e],
        [i, n, s, t, a, g],
        [i, t],
        [d, e, s, t, r, o, y],
        [m, n],
        [y, e, s, r, a, k],
        [f, i, n, t],
        [a, n, d],
        [s, t, e, g, e, l],
        [y, y, y],
        [i, c, u],
        [o, m, g],
        [t, h, e, r],
        [e, a, r, e],
        [s, o],
        [m, a, n, y],
        [w, o, r, d, s],
        [w, o, n, d, e, r, i],
        [i, f],
        [p, r, o, l, o, g],
        [c, a, n],
        [s, o, l, v],
        [t, h, i, s],
        [o, r],
        [n, o, t],
        [m, i, g, h, t],
        [u, h, e, r, e],
        [f, o, r],
        [a, w, h, i, l, e],
        [t, i, l],
        [n, o, o],
        [o, p, a, l],
        [h, e, y],
        [m, a, c, b, o, o, k],
        [b, e],
        [g, o, n, e],
        [t, r, a, i, t, o, r],
        [f, i, l, t],
        [e, e, l],
        [l, l, l],
        [d, e, l, i, r, i, o, u, s],
        [o, p],
        [l, o, v, e, l, y],
        [b, r, a, i, n],
        [y, u, m, m, y, y],
        [i, x],
        [i, n, i, t, i, a, l],
        [d, r, i, v, e],
        [b, l, a],
        [e, v, o, l, u, t, i, o, n],
        [t, o, p, i, g, h, t],
        [b, a],
        [v, o, l, v, o],
        [o, n, l, i, n, e],
        [t, a, c, t, i, c, a],
        [a, m, b, i, e, n, t],
        [l, i, g, h, t, i, n, g],
        [h, o, m],
        [b, e, e, s],
        [d, i, s, p, l, a, y],
        [l, i, g, h, t, n, i],
        [l, o, v, e],
        [d, e, a, t, h],
        [h, a, t, r, e, d],
        [c, o],
        [e, v, o],
        [k, a, r, m, a],
        [r, e, g, r, e, t],
        [s, h, a],
        [p, o, s, i, t, i, v, e],
        [e, n, e, r, g, y],
        [t, r, e],
        [h, a, r, d, l, i, n, e],
        [b, a, t, t, l, e, f, i, e, l, d],
        [a, c, c, d, e, e, i, l, o, o, p, q, r, z, w, y, a, b, b, e],
        [p, l, a, t, f, o, r, m],
        [c, o, n, n, e, c, t, o, r],
        [p, r, o, o, f],
        [c, o, n, c, e, p, t],
        [q, u, a, l, i, t],
        [y, u, m],
        [t, l, d, r],
        [a, l, m, o, s, t],
        [f, i, n, i],
        [t, e, l, e, p, o, r, t],
        [u, m, b, r, e, l, l, a],
        [a, d, v, e, r, t, i, s, e, m, e, n, t],
        [s, k, i, p, p, a],
        [h, u, m, a, n],
        [r, e, s, o, u, r, c, e],
        [c, o, m, p, a],
        [h, o, p],
        [u, n, i, v, e, r, s, i, t, y],
        [l, e, a, r, n],
        % Vertical
        [t, e, r, r, i, f, i, e, d],
        [u, n, b, e, l, i, e, v, a, b, l, e],
        [h, a, p, p, y],
        [a, h, h],
        [e, s, t, a, t, i, c],
        [t, h, o, e, e, o, x, v, o, m, e, o, v, p, a, c, l, r, u, t, d, u, o],
        [e, m, o, t],
        [n, u, m, i, h, e, o],
        [l, v],
        [o, l, b, e, v, o, o, r, c, a, o, m, e, v, m, p],
        [t, o, p],
        [d, t],
        [a, f, i, r],
        [e, i, l, v, i, s, e],
        [s, d, d, t, o],
        [l, e, a],
        [h, i],
        [h, e],
        [o, n],
        [s, e, o, o, l, l, n, u, o, e],
        [k, i, l, e, f, f, t, e, r, n, u],
        [c, e, s, a, m, y, p],
        [p, n, l, y, i, t],
        [n, d, d, a, t, i, e, o],
        [l, p, t],
        [a, g, a, l, t, n, g],
        [r, o, f, a, e, l],
        [t, i, o, t, i, e, r, i, n, i, r, c, d, o, i, r, i],
        [m, r, r, p, r, d],
        [w, o, r, o, l],
        [b, i, o, n],
        [s, a, m, v, e, l, m, o, r, r, s, e, v],
        [a, e, d],
        [t, o, l],
        [t, d, r, a, n, l, l, p, t, a, e],
        [t, e, s, e],
        [z, a],
        [m, y, s, h, r, o, n],
        [h, r, e, a, l],
        [i, i, l, h],
        [b, o, c, c, a],
        [m, o, r],
        [i, t, u, e],
        [t, e, d, g, o, a, e, a, l, i],
        [t, n, g, a],
        [r, e, a, p, o, e, l, u, e, u, s],
        [m, e, r, s],
        [t, w, y, i, i, n, d, o, e, h, y, h, e, n, t, q, n, p, m, m, n, r, i],
        [g, a, m, i, n, g],
        [t, r],
        [r, p],
        [a, g, e, t, r, n, t, o, b, t, c, t],
        [m, m, n],
        [e, e, w, a, m, i, m, o, i, y, i, i, t, i, l, t, r, r, l, z, e],
        [s, r],
        [e, y],
        [m, s, y, l, a, o, n, i, l, a, r, o, u, v, g, a, n, i, r, e, g, e, w, c, q, t, e, s],
        [i, z],
        [t, e],
        [r, n],
        [g, e, c],
        [u, m, e, h, c, g, g, e, t, y, f, y, t, u],
        [l, k, c, l],
        [g, e, k, a, s, y, e, d, s, h],
        [b, f, s, m],
        [t, t],
        [h, d],
        [i, a, o, a, f, l, i, o, e],
        [h, u, n, g, r, y],
        [e, o, t, t, o, i],
        [y, b],
        [i, h, t],
        [s, t, e, b, r, l, i, a, p, m, a],
        [u, e],
        [a, y, s, r, l],
        [i, o, l, o, y, l, b, c, o, n, c, h, r, l, b],
        [i, n],
        [p, p, r],
        [w, r, e, c, k],
        [o, i, v, b, l, k, t, p],
        [a, a, a, m, i, o, a, e, d, e],
        [t, i],
        [a, a, n]
    ],
    copy_term(Puzzle, Sol),
    findall(Sol, puzzle_solution(Sol, WordList), Solutions),
    maplist(print_puzzle, Solutions),
    ExpectedPuzzle = [
        [ t,  e,  e,  t,  h, '#', a,  m,  a,  z,  i,  n,  g, '#', h,  i,  g,  h, '#', w ],
        [ e,  s,  m,  o,  i, '#', g,  r,  e,  a,  t, '#', a,  m, '#', z,  e,  u,  u,  r ],
        [ r,  t,  o,  p, '#', c,  a,  r,  d, '#', u,  m,  m,  m,  m, '#', k,  n,  e,  e ],
        [ r,  a,  t, '#', h,  e,  l,  p, '#', m,  e, '#', i,  n,  s,  t,  a,  g, '#', c ],
        [ i,  t, '#', d,  e,  s,  t,  r,  o,  y, '#', m,  n, '#', y,  e,  s,  r,  a,  k ],
        [ f,  i,  n,  t, '#', a,  n,  d, '#', s,  t,  e,  g,  e,  l, '#', y,  y,  y, '#'],
        [ i,  c,  u, '#', o,  m,  g, '#', t,  h,  e,  r, '#', e,  a,  r,  e, '#', s,  o ],
        [ e, '#', m,  a,  n,  y, '#', w,  o,  r,  d,  s, '#', w,  o,  n,  d,  e,  r,  i ],
        [ d, '#', i,  f, '#', p,  r,  o,  l,  o,  g, '#', c,  a,  n, '#', s,  o,  l,  v ],
        ['#', t,  h,  i,  s, '#', o,  r, '#', n,  o,  t, '#', m,  i,  g,  h,  t, '#', b ],
        [ u,  h,  e,  r,  e, '#', f,  o,  r, '#', a,  w,  h,  i,  l,  e, '#', t,  i,  l ],
        [ n,  o,  o, '#', o,  p,  a,  l, '#', h,  e,  y, '#', m,  a,  c,  b,  o,  o,  k ],
        [ b,  e, '#', g,  o,  n,  e, '#', t,  r,  a,  i,  t,  o,  r, '#', f,  i,  l,  t ],
        [ e,  e,  l, '#', l,  l,  l, '#', d,  e,  l,  i,  r,  i,  o,  u,  s, '#', o,  p ],
        [ l,  o,  v,  e,  l,  y, '#', b,  r,  a,  i,  n, '#', y,  u,  m,  m,  y,  y, '#'],
        [ i,  x, '#', i,  n,  i,  t,  i,  a,  l, '#', d,  r,  i,  v,  e, '#', b,  l,  a ],
        [ e,  v,  o,  l,  u,  t,  i,  o,  n, '#', t,  o,  p,  i,  g,  h,  t, '#', b,  a ],
        [ v,  o,  l,  v,  o, '#', o,  n,  l,  i,  n,  e, '#', t,  a,  c,  t,  i,  c,  a ],
        [ a,  m,  b,  i,  e,  n,  t, '#', l,  i,  g,  h,  t,  i,  n,  g, '#', h,  o,  m ],
        [ b,  e,  e,  s, '#', d,  i,  s,  p,  l,  a,  y, '#', l,  i,  g,  h,  t,  n,  i ],
        [ l,  o,  v,  e, '#', d,  e,  a,  t,  h, '#', h,  a,  t,  r,  e,  d, '#', c,  o ],
        [ e,  v,  o, '#', k,  a,  r,  m,  a, '#', r,  e,  g,  r,  e,  t, '#', s,  h,  a ],
        ['#', p,  o,  s,  i,  t,  i,  v,  e, '#', e,  n,  e,  r,  g,  y, '#', t,  r,  e ],
        [ h,  a,  r,  d,  l,  i,  n,  e, '#', b,  a,  t,  t,  l,  e,  f,  i,  e,  l,  d ],
        [ a,  c,  c,  d,  e,  e,  i,  l,  o,  o,  p,  q,  r,  z,  w,  y,  a,  b,  b,  e ],
        [ p,  l,  a,  t,  f,  o,  r,  m, '#', c,  o,  n,  n,  e,  c,  t,  o,  r, '#','#'],
        [ p,  r,  o,  o,  f, '#', c,  o,  n,  c,  e,  p,  t, '#', q,  u,  a,  l,  i,  t ],
        [ y,  u,  m, '#', t,  l,  d,  r, '#', a,  l,  m,  o,  s,  t, '#', f,  i,  n,  i ],
        ['#', t,  e,  l,  e,  p,  o,  r,  t, '#', u,  m,  b,  r,  e,  l,  l,  a, '#','#'],
        [ a,  d,  v,  e,  r,  t,  i,  s,  e,  m,  e,  n,  t, '#', s,  k,  i,  p,  p,  a ],
        [ h,  u,  m,  a,  n, '#', r,  e,  s,  o,  u,  r,  c,  e, '#', c,  o,  m,  p,  a ],
        [ h,  o,  p, '#', u,  n,  i,  v,  e,  r,  s,  i,  t,  y, '#', l,  e,  a,  r,  n ]
    ],
    debug(puzzle, '>>> 32x20 PUZZLE TEST PASSED <<<', []).

:- end_tests(puzzle_solution).
