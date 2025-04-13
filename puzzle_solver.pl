:- module(puzzle_solver, [puzzle_solution/2]).
:- use_module(library(clpfd), [transpose/2]).
:- use_module(library(plunit)).

/*********************************************************************
 * Author:   [YOUR_NAME_HERE] <[YOUR_EMAIL_HERE]>
 * Purpose:  Crossword puzzle solver for automatic filling of crossword
 *           grids using a dictionary of words
 *
 * This module provides a solution to crossword puzzles by placing words
 * from a given word list into a crossword grid. The algorithm uses a
 * dynamic "most constrained slot first" approach to efficiently search
 * for a solution, making it more effective than naive approaches.
 *
 * Input:
 *   Puzzle = list-of-lists representing the grid, where:
 *     - '#' represents a blocked cell
 *     - A letter represents a pre-filled cell
 *     - A variable (var(_)) represents an empty cell to be filled
 *   WordList = list of words, each word is a list of letters
 *
 * Output:
 *   The Puzzle with all empty cells filled with letters from WordList
 *********************************************************************/

/*********************************************************************
 * puzzle_solution/2
 *
 * The main entry point: 
 *   Puzzle = list-of-lists, each cell either '#', a letter, or var(_).
 *   WordList = list of words, each word is a list of letters.
 *
 * Algorithm steps:
 *  1) Build dictionary: length -> [words of that length].
 *  2) Extract all horizontal & vertical slots. Store as a simple list.
 *  3) fill_puzzle(Puzzle, Slots, WordDict):
 *      - If no more slots => must check WordDict is empty => success/fail
 *      - Else pick the slot with the fewest candidate words => fill it
 *        by trying each candidate. If all fail => backtrack.
 *  4) A candidate word is one that matches length & partial puzzle letters.
 *  5) Each time we place a word in a slot, we remove that word from
 *     the dictionary (since each word can only be used once).
 *********************************************************************/
puzzle_solution(Puzzle, WordList) :-
    /* 1) Build dictionary of words by length. */
    build_word_dict(WordList, WordDict0),
    
    /* 2) Extract all slots (>= 2 in length). */
    extract_all_slots(Puzzle, Slots),
    
    /* 3) Solve by dynamically picking the most constrained slot first. */
    fill_puzzle(Puzzle, Slots, WordDict0).

/*********************************************************************
 * build_word_dict/2
 * Build a map (list of length->words) from the given WordList.
 *
 * Arguments:
 *   WordList: The list of words to organize
 *   WordDict: The resulting dictionary as a list of pairs (Length-[Words])
 *
 * Example: If WordList = [[c,a,t],[b,a,g],[i,t],[i,f]]
 *   Then WordDict might be:
 *   [
 *     2 -> [[i,t],[i,f]],
 *     3 -> [[c,a,t],[b,a,g]]
 *   ]
 *
 * Implementation detail:
 *   We store it as a list of pairs: [ length-words, ... ].
 *   We can look up by length with 'get_words_of_length/3'.
 *********************************************************************/
build_word_dict(WordList, WordDict) :-
    /* Group words by length using an accumulator approach. */
    group_words_by_length(WordList, [], WordDictUnsorted),
    /* Sort the dictionary by length ascending (not crucial, but neat). */
    sort(WordDictUnsorted, WordDict).

/*********************************************************************
 * group_words_by_length/3
 * Groups words by their length recursively
 *
 * Arguments:
 *   WordList: Remaining words to be processed
 *   Acc: Accumulator of grouped words so far
 *   Out: Final grouped dictionary
 *********************************************************************/
group_words_by_length([], Acc, Acc).
group_words_by_length([W|Ws], Acc, Out) :-
    length(W, L),
    /* Insert W into the bucket for length L. */
    insert_word_in_dict(L, W, Acc, Acc2),
    group_words_by_length(Ws, Acc2, Out).

/*********************************************************************
 * insert_word_in_dict/4
 * Inserts a word into the appropriate length bucket in the dictionary
 *
 * Arguments:
 *   L: Length of the word
 *   W: The word to insert
 *   DictIn: Input dictionary
 *   DictOut: Dictionary after insertion
 *
 * If there's a pair (L->List), we add W to that list,
 * else we create a new pair.
 *********************************************************************/
insert_word_in_dict(L, W, [], [L-[W]]) :- !.
insert_word_in_dict(L, W, [Len-Words|Rest], [Len-[W|Words]|Rest]) :-
    L == Len, !.
insert_word_in_dict(L, W, [Pair|Rest], [Pair|NewRest]) :-
    insert_word_in_dict(L, W, Rest, NewRest).

/*********************************************************************
 * extract_all_slots/2
 * Gathers horizontal & vertical runs (>=2) of open cells.
 *
 * Arguments:
 *   Puzzle: The crossword grid
 *   AllSlots: List of all slots found (horizontal and vertical)
 *
 * A slot is represented as slot(Orientation, Length, Coordinates)
 * where Orientation is 'horizontal' or 'vertical', Length is the
 * number of cells, and Coordinates is a list of (Row,Col) pairs.
 *********************************************************************/
extract_all_slots(Puzzle, AllSlots) :-
    writeln('Extracting slots...'),
    extract_horizontal_slots(Puzzle, HSlots),
    writeln('Horizontal slots:'), writeln(HSlots),
    extract_vertical_slots(Puzzle, VSlots),
    writeln('Vertical slots:'), writeln(VSlots),
    append(HSlots, VSlots, AllSlots),
    writeln('All slots:'), writeln(AllSlots).

/*********************************************************************
 * extract_horizontal_slots/2
 * Finds all horizontal slots in the puzzle
 *
 * Arguments:
 *   Puzzle: The crossword grid
 *   HSlots: List of horizontal slots found
 *********************************************************************/
extract_horizontal_slots(Puzzle, HSlots) :-
    length(Puzzle, NumRows),
    format("NumRows = ~w~n", [NumRows]),
    NumRowsMinus1 is NumRows - 1,

    findall(RowSlots,
      ( between(0, NumRowsMinus1, RowIndex),
        format("RowIndex = ~w~n", [RowIndex]),
        nth0(RowIndex, Puzzle, Row),
        format("Row = ~w~n", [Row]),
        row_to_slots(Row, RowIndex, RowSlots)
      ),
      SlotsPerRow),

    flatten(SlotsPerRow, HSlots).

/*********************************************************************
 * row_to_slots/3
 * Converts a row to a list of horizontal slots
 *
 * Arguments:
 *   RowList: List of cells in the row
 *   RowIndex: Index of the row in the puzzle
 *   RowSlots: List of slots found in this row
 *********************************************************************/
row_to_slots(RowList, RowIndex, RowSlots) :-
    collect_runs(RowList, 0, Runs),
    make_horizontal_slots(Runs, RowIndex, RowSlots).

/*********************************************************************
 * collect_runs/3
 * Collects consecutive runs of non-blocked cells in a row
 *
 * Arguments:
 *   List: List of cells to process
 *   Col: Current column position
 *   Runs: List of runs found, each run is a list of column indices
 *********************************************************************/
collect_runs([], _, []).
collect_runs([Cell|T], Col, Runs) :-
    Cell == '#',  % blocked => skip
    writeln(['Blocked cell at', Col]),
    Col1 is Col + 1,
    collect_runs(T, Col1, Runs).

collect_runs([Cell|T], Col, [[Col|MoreCols] | OtherRuns]) :-
    Cell \== '#',  % open cell
    format("Open cell at column ~w: ~w~n", [Col, Cell]),
    writeln(['Cell', Cell]),
    ColPlusOne is Col + 1,
    collect_consecutive(T, ColPlusOne, MoreCols, Remainder),
    length(MoreCols, RunLength),
    writeln(['ColPlusOne:', ColPlusOne]),
    writeln(['RunLength:', RunLength]),
    % ColPlusOne is the starting column of the consecutive run
    % RunLength is the length of the consecutive run
    % + 1 for the next column after the consecutive run terminates
    NextCol is ColPlusOne + RunLength + 1,
    writeln(['NextCol:', NextCol]),
    collect_runs(Remainder, NextCol, OtherRuns).

/*********************************************************************
 * collect_consecutive/4
 * Collects consecutive non-blocked cells
 *
 * Arguments:
 *   List: List of cells to process
 *   Col: Current column position
 *   MoreCols: Additional column indices in this run
 *   Remainder: Remaining cells after this run
 *********************************************************************/
collect_consecutive([], _Col, [], []).
collect_consecutive([Cell|T], Col, [], T) :-
    Cell == '#', 
    format("Terminating run at blocked cell in column ~w~n", [Col]),
    !.
collect_consecutive([Cell|T], Col, [Col|More], Remainder) :-
    Cell \== '#',
    format("Collecting consecutive open cell at column ~w~n", [Col]),
    Col1 is Col + 1,
    collect_consecutive(T, Col1, More, Remainder).

/*********************************************************************
 * make_horizontal_slots/3
 * Converts runs of column indices to horizontal slots
 *
 * Arguments:
 *   RunCols: List of runs, each run is a list of column indices
 *   Row: Row index for this slot
 *   Slots: List of slots created
 *
 * Only creates slots for runs with length >= 2
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
 * Finds all vertical slots in the puzzle by transposing the grid
 *
 * Arguments:
 *   Puzzle: The crossword grid
 *   VSlots: List of vertical slots found
 *********************************************************************/
extract_vertical_slots(Puzzle, VSlots) :-
    transpose(Puzzle, TGrid),
    extract_horizontal_slots(TGrid, TempHSlots),
    fix_transposed_coords(TempHSlots, VSlots).

/*********************************************************************
 * fix_transposed_coords/2
 * Converts horizontal slots in transposed grid to vertical slots
 *
 * Arguments:
 *   TempHSlots: Horizontal slots in transposed grid
 *   VSlots: Resulting vertical slots in original grid
 *********************************************************************/
fix_transposed_coords([], []).
fix_transposed_coords([slot(horizontal,Len,TC)|T],[slot(vertical,Len,OC)|T2]) :-
    maplist(swap_rc, TC, OC),
    fix_transposed_coords(T, T2).

/*********************************************************************
 * swap_rc/2
 * Swaps row and column coordinates
 *
 * Arguments:
 *   (R,C): Input row-column pair
 *   (C,R): Output column-row pair (swapped)
 *********************************************************************/
swap_rc((R,C),(C,R)).

/*********************************************************************
 * get_words_of_length/3
 * Retrieve the list of words for 'Length' from the dictionary.
 *
 * Arguments:
 *   Dict: The word dictionary
 *   L: Length of words to retrieve
 *   WordsList: List of words of length L (empty if none found)
 *********************************************************************/
get_words_of_length([], _L, []).
get_words_of_length([Len-Words|_], L, Words) :- L == Len, !.
get_words_of_length([_|Rest], L, Words) :-
    get_words_of_length(Rest, L, Words).

/*********************************************************************
 * remove_word_from_dict/4
 * Remove 'Word' from the dictionary bucket for 'Length'.
 *
 * Arguments:
 *   DictIn: Input dictionary
 *   L: Length of the word
 *   Word: The word to remove
 *   DictOut: Dictionary after removal
 *
 * If that bucket becomes empty afterwards, remove that pair entirely.
 *********************************************************************/
remove_word_from_dict([], _L, _Word, []) :- 
    /* Should not happen if we keep consistent usage. */
    fail.

remove_word_from_dict([Len-Words|Rest], L, Word, [Len-NewWords|Rest]) :-
    L == Len, 
    select(Word, Words, NewWords), 
    NewWords \= [], 
    !.

remove_word_from_dict([Len-[Word]|Rest], L, Word, Rest) :-
    /* If removing Word from that single-element bucket => remove bucket. */
    L == Len, !.

remove_word_from_dict([Pair|Rest], L, W, [Pair|NewRest]) :-
    remove_word_from_dict(Rest, L, W, NewRest).

/*********************************************************************
 * fill_puzzle/3
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
 *     place it, remove from dict, recurse. On failure => backtrack.
 *********************************************************************/
fill_puzzle(_Puzzle, [], WordDict) :-
    /* If no more slots, we succeed only if no leftover words remain. */
    ( WordDict == [] ->
        writeln('All slots filled successfully.'),
        !
      ; writeln(['No more slots, but leftover words remain', WordDict, ' => fail']),
        fail
    ).

fill_puzzle(Puzzle, Slots, WordDict) :-
    /* 1) Pick the slot with the fewest candidate words => "most constrained" */
    pick_most_constrained_slot(Puzzle, Slots, WordDict, Slot, OtherSlots),
    /* 2) Attempt each candidate word in turn. */
    try_fill_slot_with_candidates(Puzzle, Slot, OtherSlots, WordDict).

/*********************************************************************
 * pick_most_constrained_slot/5
 * Evaluate candidate count for each slot => pick the one with min #.
 *
 * Arguments:
 *   Puzzle: The crossword grid
 *   Slots: List of slots to evaluate
 *   WordDict: Dictionary of available words
 *   Slot: Selected slot with fewest candidates
 *   OtherSlots: Remaining slots (excluding the selected one)
 *********************************************************************/
pick_most_constrained_slot(Puzzle, [S|Ss], WordDict, Slot, OtherSlots) :-
    /* Evaluate # of candidates for S. */
    candidates_for_slot(Puzzle, S, WordDict, CandS),
    length(CandS, CountS),
    pick_most_constrained_slot_aux(Puzzle, Ss, WordDict, S, CountS, Slot),
    /* Remove that Slot from the big list => OtherSlots is the rest. */
    delete([S|Ss], Slot, OtherSlots).

/*********************************************************************
 * pick_most_constrained_slot_aux/6
 * Helper to find the slot with the fewest candidate words
 *
 * Arguments:
 *   Puzzle: The crossword grid
 *   Slots: Remaining slots to check
 *   WDict: Dictionary of available words
 *   BestSlotSoFar: Best slot found so far
 *   BestCount: Number of candidates for best slot so far
 *   FinalSlot: Final selected slot (output)
 *********************************************************************/
pick_most_constrained_slot_aux(_Puzzle, [], _WDict, Slot, _Count, Slot).
pick_most_constrained_slot_aux(Puzzle, [S2|Ss], WDict, BestSlotSoFar, BestCount, FinalSlot) :-
    candidates_for_slot(Puzzle, S2, WDict, Cand2),
    length(Cand2, Count2),
    ( Count2 < BestCount ->
        pick_most_constrained_slot_aux(Puzzle, Ss, WDict, S2, Count2, FinalSlot)
    ; pick_most_constrained_slot_aux(Puzzle, Ss, WDict, BestSlotSoFar, BestCount, FinalSlot)
    ).

/*********************************************************************
 * candidates_for_slot/4
 * Returns all valid candidate words from WordDict that:
 *   (1) match the slot's length,
 *   (2) don't conflict with puzzle's partially filled letters.
 *
 * Arguments:
 *   Puzzle: The crossword grid
 *   slot(_Orient, Len, Coords): The slot to find candidates for
 *   WordDict: Dictionary of available words
 *   Candidates: List of valid candidates for this slot
 *********************************************************************/
candidates_for_slot(Puzzle, slot(_Orient, Len, Coords), WordDict, Candidates) :-
    /* 1) Get all words for 'Len' from dictionary. */
    get_words_of_length(WordDict, Len, WordCandidates),
    writeln(['Word candidates for length', Len, '=>', WordCandidates]),
    /* 2) Filter out invalid candidates. */
    include(validate_candidate(Puzzle, Coords), WordCandidates, Candidates),
    writeln(['Valid candidates after check:', Candidates, 'for slot with coords', Coords]).

/*********************************************************************
 * validate_candidate/3
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
 * Checks if a word is compatible with existing letters in the grid
 *
 * Arguments:
 *   Puzzle: The crossword grid
 *   Coords: List of (Row,Col) coordinates to check
 *   Word: List of letters to validate
 *
 * A word is valid if:
 * - For empty cells (var), any letter is allowed
 * - For filled cells, the letter must match what's already there
 *********************************************************************/
validate_cells(_Puzzle, [], []).  % Base case: no more cells or letters to check.
validate_cells(Puzzle, [(R, C) | Tcoords], [Letter | Rest]) :-
    nth0(R, Puzzle, Row),
    nth0(C, Row, Cell),
    (   var(Cell) -> true  % Cell is unbound, valid
    ;   Cell == Letter -> true  % Cell already contains the correct letter
    ),
    validate_cells(Puzzle, Tcoords, Rest).  % Recurse for the remaining cells and letters.

/*********************************************************************
 * try_fill_slot_with_candidates/4
 * Attempt each candidate word in turn. If all fail => backtrack.
 *
 * Arguments:
 *   Puzzle: The crossword grid
 *   Slot: The slot to fill
 *   OtherSlots: Remaining slots to fill
 *   WordDict: Dictionary of available words
 *
 * If a candidate fails, we backtrack and try the next one.
 *********************************************************************/
try_fill_slot_with_candidates(Puzzle, Slot, OtherSlots, WordDict) :-
    /* Evaluate candidates for Slot. */
    candidates_for_slot(Puzzle, Slot, WordDict, [Word|_]),
    /* If there's at least one Word => place it. */
    place_word_in_slot(Puzzle, Slot, Word),
    writeln(['Placing word =>', Word, 'in slot =>', Slot]),
    /* Remove from dict. */
    Slot = slot(_, Len, _),
    remove_word_from_dict(WordDict, Len, Word, WordDict2),
    /* Recurse => fill the rest of the slots. */
    fill_puzzle(Puzzle, OtherSlots, WordDict2)
    /* If we succeed => done. */
    ;
    /* If we fail => try next candidate in the list => backtrack approach. */
    candidates_for_slot(Puzzle, Slot, WordDict, [_|RestCand]),
    RestCand \== [],
    writeln('Current candidate failed => trying next candidate.'),
    fail.

/*********************************************************************
 * place_word_in_slot/3
 * Unifies puzzle cells with Word letters
 *
 * Arguments:
 *   Puzzle: The crossword grid
 *   slot(_Orient,_Len,Coords): The slot to fill
 *   Word: The word to place in the slot
 *
 * No need to revert if we fail because Prolog automatically backtracks
 * these unifications.
 *********************************************************************/
place_word_in_slot(_Puzzle, slot(_Orient,_Len,[]), []).
place_word_in_slot(Puzzle, slot(O,L,[(R,C)|Coords]), [Letter|Rest]) :-
    nth0(R, Puzzle, Row),
    nth0(C, Row, Cell),
    ( var(Cell) ->
        Cell = Letter
    ; true  /* If it's already that letter => okay, else fail in conflict check. */
    ),
    place_word_in_slot(Puzzle, slot(O,L,Coords), Rest).


print_puzzle(Puzzle) :-
    writeln('Completed Puzzle:'),
    maplist(print_row, Puzzle).

print_row(Row) :-
    writeln(Row).

/*********************************************************************
 * Tests
 *********************************************************************/
:- begin_tests(puzzle_solution).

/* Test Case 1: example */
test(example,[nondet]) :-
    Puzzle = [
        ['#', h, '#'],
        [ _,  _,  _ ],
        ['#', _, '#']
    ],
    WordList = [
        [h, a, t],
        [b, a, g]
    ],
    puzzle_solution(Puzzle, WordList),
    print_puzzle(Puzzle),  % Print the completed puzzle
    ExpectedPuzzle = [
        ['#', h, '#'],
        [ b,  a,  g ],
        ['#', t, '#']
    ],
    assertion(Puzzle == ExpectedPuzzle).

/* Test Case 2: small_puzzle */
test(small_puzzle,[nondet]) :-
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
    puzzle_solution(Puzzle, WordList),
    print_puzzle(Puzzle),  % Print the completed puzzle
    ExpectedPuzzle = [
        [ e, '#', i ],
        [ c,  a,  t ],
        ['#', r, '#' ]
    ],
    assertion(Puzzle == ExpectedPuzzle).

/* Test Case 3: wikipedia puzzle */
test(wikipedia, [nondet]) :-
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
    puzzle_solution(Puzzle, WordList),
    print_puzzle(Puzzle),  % Print the completed puzzle
    ExpectedPuzzle = [
        ['#', '#', '#',  e ,  v ,  o , '#', '#', '#', '#'],
        ['#', '#', '#',  d ,  e ,  n ,  s , '#', '#', '#'],
        ['#',  d ,  a ,  i ,  s , '#',  i ,  o , '#', '#'],
        ['#',  a ,  r ,  t ,  i ,  c ,  l ,  e , '#', '#'],
        ['#',  g ,  i , '#',  c ,  l ,  o ,  d , '#', '#'],
        ['#', '#',  d ,  o ,  l ,  e , '#', '#', '#', '#'],
        ['#', '#', '#',  r ,  e ,  f , '#', '#', '#', '#']
    ],
    assertion(Puzzle == ExpectedPuzzle).

/* Test Case 4: More slots than words (should fail) */
test(more_slots_than_words, [fail]) :-
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
    puzzle_solution(Puzzle, WordList),
    print_puzzle(Puzzle).  % Print the completed puzzle

/* Test Case 5: More words than slots (should fail) */
test(more_words_than_slots, [fail]) :-
    Puzzle = [
        ['#', _, '#'],
        ['#', _, '#'],
        ['#', _, '#']
    ],
    WordList = [
        [a],
        [b],
        [c],
        [d]
    ],
    puzzle_solution(Puzzle, WordList),
    print_puzzle(Puzzle).  % Print the completed puzzle

/* Test Case 6: Most constrained slot challenge */
test(most_constrained_slot, [nondet]) :-
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
    puzzle_solution(Puzzle, WordList),
    print_puzzle(Puzzle),  % Print the completed puzzle
    ExpectedPuzzle = [
        [ c,  a,  r,  t ],
        [ z,  x,  a,  r ],
        [ h,  a,  t,  e ],
        [ c,  k,  e,  e ]
    ],
    assertion(Puzzle == ExpectedPuzzle).

:- end_tests(puzzle_solution).
