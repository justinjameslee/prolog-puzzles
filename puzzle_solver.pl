:- module(puzzle_solver, [puzzle_solution/2]).
:- use_module(library(clpfd), [transpose/2]).
:- use_module(library(plunit)).

/*********************************************************************
 * puzzle_solution/2
 *
 * The main entry point: Puzzle is a list-of-lists, each cell is '#'
 * (blocked), a letter atom, or an unbound variable (_). WordList is
 * a list of word-lists, e.g. [ [c,a,t], [b,a,g], ... ].
 *
 * The solver:
 *  (1) Extracts all horizontal & vertical slots from Puzzle.
 *  (2) Sorts those slots by how many letters are already prefilled.
 *  (3) Fills each slot exactly once, removing each used word from WordList.
 *  (4) Ensures no cell is overwritten incorrectly (partial letters must match).
 *********************************************************************/
puzzle_solution(Puzzle, WordList) :-
    extract_all_slots(Puzzle, UnorderedSlots),
    sort_slots_by_prefilled(Puzzle, UnorderedSlots, Slots),
    fill_slots(Slots, Puzzle, WordList).

/*********************************************************************
 * extract_all_slots/2
 * Gathers horizontal & vertical runs (>=2) of open cells.
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
 * For each row, find runs of length >= 2 (skip single '#' or single cells).
 *********************************************************************/
extract_horizontal_slots(Puzzle, HSlots) :-
    length(Puzzle, NumRows),
    format("NumRows = ~w~n", [NumRows]),
    NumRowsMinus1 is NumRows - 1,

    % For each row, find consecutive open cells
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
 * Convert a single row's runs of columns into slot(horizontal,Len,Coords).
 *********************************************************************/
row_to_slots(RowList, RowIndex, RowSlots) :-
    collect_runs(RowList, 0, Runs),
    make_horizontal_slots(Runs, RowIndex, RowSlots).

/*********************************************************************
 * collect_runs/3
 * Identifies consecutive open cells (# is blocked). 
 * Each run is a list of column indices.
 *********************************************************************/
collect_runs([], _, []).

collect_runs([Cell|T], Col, Runs) :-
    Cell == '#',
    % blocked cell => move on
    Col1 is Col + 1,
    collect_runs(T, Col1, Runs).

collect_runs([Cell|T], Col, [[Col|MoreCols] | OtherRuns]) :-
    Cell \== '#',  % open cell (var or letter)
    ColPlusOne is Col + 1,
    collect_consecutive(T, ColPlusOne, MoreCols, Remainder),
    length(MoreCols, RunLength),
    NextCol is Col + RunLength + 1,
    collect_runs(Remainder, NextCol, OtherRuns).

/*********************************************************************
 * collect_consecutive/4
 * Accumulates subsequent columns as long as they're not '#'.
 *********************************************************************/
collect_consecutive([], _Col, [], []) :- !.

collect_consecutive([Cell|T], _Col, [], T) :-
    Cell == '#',
    !.

collect_consecutive([Cell|T], Col, [Col|More], Remainder) :-
    Cell \== '#',
    Col1 is Col + 1,
    collect_consecutive(T, Col1, More, Remainder).

/*********************************************************************
 * make_horizontal_slots/3
 * Build slot(horizontal,Len,Coords) if run length >=2.
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
 * Transpose puzzle => each column => a "row" => reuse horizontal code.
 *********************************************************************/
extract_vertical_slots(Puzzle, VSlots) :-
    transpose(Puzzle, TGrid),
    extract_horizontal_slots(TGrid, TempHSlots),
    fix_transposed_coords(TempHSlots, VSlots).

/*********************************************************************
 * fix_transposed_coords/2
 * Convert slot(horizontal,Len,TransCoords) => slot(vertical,Len,OrigCoords).
 *********************************************************************/
fix_transposed_coords([], []).
fix_transposed_coords(
    [slot(horizontal,Len,TransCoords)|T1],
    [slot(vertical,Len,OrigCoords)|T2]
) :-
    maplist(swap_rc, TransCoords, OrigCoords),
    fix_transposed_coords(T1, T2).

swap_rc((R,C), (C,R)).

/*********************************************************************
 * sort_slots_by_prefilled/3
 * For each slot, count how many puzzle cells are already letters.
 * Then sort by that count in descending order (more prefilled => earlier).
 *********************************************************************/
sort_slots_by_prefilled(_, [], []).

sort_slots_by_prefilled(Puzzle, [Slot|Others], SortedSlots) :-
    slot_prefilled_count(Puzzle, Slot, Count),
    sort_slots_by_prefilled(Puzzle, Others, Rest),
    insert_by_count(Slot-Count, Rest, SortedSlots).

/*********************************************************************
 * slot_prefilled_count/3
 * Count how many cells in this slot are already a letter
 * (not var(Cell), not '#').
 *********************************************************************/
slot_prefilled_count(_Puzzle, slot(_Orient,_Len,[]), 0).
slot_prefilled_count(Puzzle, slot(Orient, Len, [(R,C)|Coords]), Count) :-
    nth0(R, Puzzle, Row),
    nth0(C, Row, Cell),
    (   var(Cell)
    ->  ThisOne = 0  % still unbound
    ;   Cell == '#'
    ->  ThisOne = 0  % blocked
    ;   ThisOne = 1  % already a letter
    ),
    slot_prefilled_count(Puzzle, slot(Orient,Len,Coords), Rest),
    Count is ThisOne + Rest.

/*********************************************************************
 * insert_by_count/3
 * Insert Slot-Count into a list sorted by descending Count.
 *********************************************************************/
insert_by_count(SC, [], [SC]).
insert_by_count(SlotA-CountA, [SlotB-CountB|Rest], [SlotA-CountA,SlotB-CountB|Rest]) :-
    CountA >= CountB, !.
insert_by_count(SC, [X|Rest], [X|NewRest]) :-
    insert_by_count(SC, Rest, NewRest).

/*********************************************************************
 * fill_slots/3
 * Fill each slot exactly once, removing used words from WordList
 *********************************************************************/
fill_slots([], _Puzzle, _WordList) :-
    writeln('All slots filled successfully.').

fill_slots([Slot-_|OtherSlots], Puzzle, WordList) :-
    Slot = slot(_,_,_),
    writeln(['Filling slot:', Slot]),
    select_a_word_and_fill(Slot, Puzzle, WordList, NewWordList),
    fill_slots(OtherSlots, Puzzle, NewWordList).

/*********************************************************************
 * select_a_word_and_fill/4
 * Try each Word in WordList. On success, remove that Word. Otherwise fail.
 *********************************************************************/
select_a_word_and_fill(Slot, Puzzle, [Word|RestWords], Rest) :-
    writeln(['Trying word', Word, 'in slot', Slot]),
    (   slot_matches_word(Puzzle, Slot, Word)
    ->  writeln(['Success check for', Word]),
        place_word_in_slot(Puzzle, Slot, Word),
        writeln(['Placed word', Word, 'in slot', Slot]),
        Rest = RestWords
    ;   writeln(['Failed check for', Word]),
        fail
    )
    ;   select_a_word_and_fill(Slot, Puzzle, RestWords, Rest).

/*********************************************************************
 * slot_matches_word/3
 * Checks if Word is correct length & no letter conflicts with puzzle.
 *********************************************************************/
slot_matches_word(Puzzle, slot(_Orient, Length, Coords), Word) :-
    writeln(['Checking if word matches slot:', Word, 'Slot coordinates:', Coords]),
    length(Word, Length),
    writeln(['Word length matches slot length:', Length]),
    check_letters_match(Puzzle, Coords, Word).

/*********************************************************************
 * check_letters_match/3
 * Each puzzle cell must be either var or match the letter of Word.
 *********************************************************************/
check_letters_match(_Puzzle, [], []) :-
    writeln('All letters matched successfully.').

check_letters_match(Puzzle, [(R,C)|Tcoords], [Letter|Tword]) :-
    nth0(R, Puzzle, Row),
    nth0(C, Row, Cell),
    writeln(['Checking cell:', (R, C), 'Expected:', Letter, 'Actual:', Cell]),
    (   var(Cell)
    ->  writeln(['Cell is unbound, matches automatically.'])
    ;   Cell == Letter
    ->  writeln(['Cell matches expected letter.'])
    ;   writeln(['Cell does not match expected letter.']),
        fail
    ),
    check_letters_match(Puzzle, Tcoords, Tword).

/*********************************************************************
 * place_word_in_slot/3
 * Actually unify puzzle cells with Word letters.
 *********************************************************************/
place_word_in_slot(_, slot(_,_,[]), []) :-
    writeln('Word placed successfully.').

place_word_in_slot(Puzzle, slot(Orient, Len, [(R,C)|Coords]), [Letter|Rest]) :-
    nth0(R, Puzzle, Row),
    nth0(C, Row, Cell),
    writeln(['Placing letter:', Letter, 'at cell:', (R, C)]),
    (   var(Cell)
    ->  Cell = Letter,
        writeln(['Cell was unbound, now set to:', Letter])
    ;   writeln(['Cell already contains:', Cell])
    ),
    place_word_in_slot(Puzzle, slot(Orient, Len, Coords), Rest).

/*********************************************************************
 * Test Cases using plunit
 *
 * The "small_puzzle" test expects the final puzzle:
 * [ e, '#', i ]
 * [ c,  a,  t ]
 * ['#', r, '#' ]
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
        [c, a, t],
        [i, t],
        [e, c],
        [a, r]
    ],
    puzzle_solution(Puzzle, WordList),
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
    /* The final puzzle after solving must match this exact arrangement: */
    Puzzle = [
        ['#', '#', '#',  e ,  v ,  o , '#', '#', '#', '#'],
        ['#', '#', '#',  d ,  e ,  n ,  s , '#', '#', '#'],
        ['#',  d ,  a ,  i ,  s , '#',  i ,  o , '#', '#'],
        ['#',  a ,  r ,  t ,  i ,  c ,  l ,  e , '#', '#'],
        ['#',  g ,  i , '#',  c ,  l ,  o ,  d , '#', '#'],
        ['#', '#',  d ,  o ,  l ,  e , '#', '#', '#', '#'],
        ['#', '#', '#',  r ,  e ,  f , '#', '#', '#', '#']
    ].

:- end_tests(puzzle_solution).
