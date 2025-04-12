:- module(puzzle_solver, [puzzle_solution/2]).
:- use_module(library(clpfd), [transpose/2]).
:- use_module(library(plunit)).

/*********************************************************************
 * puzzle_solution/2
 * Puzzle is a list-of-lists, each cell is '#' or a letter or _ (var).
 * WordList is a list of word-lists, like [ [c,a,t], [i,t], ... ].
 *********************************************************************/
puzzle_solution(Puzzle, WordList) :-
    extract_all_slots(Puzzle, Slots),
    fill_slots(Slots, Puzzle, WordList).

/*********************************************************************
 * extract_all_slots/2
 * Finds horizontal + vertical slots (runs of open cells >= 2).
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
 * For each row, find runs of length >= 2 (skip blocks '#').
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
 * Convert one row into a list of slot(horizontal,Length,Coords).
 *********************************************************************/
row_to_slots(RowList, RowIndex, RowSlots) :-
    collect_runs(RowList, 0, Runs),
    make_horizontal_slots(Runs, RowIndex, RowSlots).

/*********************************************************************
 * collect_runs/3
 * Recursively gather consecutive open cells (Cell \= '#').
 *********************************************************************/
collect_runs([], _, []).

collect_runs([Cell|T], Col, Runs) :-
    Cell == '#',
    Col1 is Col + 1,
    collect_runs(T, Col1, Runs).

collect_runs([Cell|T], Col, [[Col|MoreCols] | OtherRuns]) :-
    var(Cell),  % Check if the cell is an open cell
    ColPlusOne is Col + 1,
    collect_consecutive(T, ColPlusOne, MoreCols, Remainder),
    length(MoreCols, RunLength),  % Calculate the length of the run
    NextCol is Col + RunLength + 1,  % Update the next column index
    collect_runs(Remainder, NextCol, OtherRuns).

/*********************************************************************
 * collect_consecutive/4
 * Gather subsequent columns while not '#'.
 *********************************************************************/
collect_consecutive([], _Col, [], []) :-
    !.

collect_consecutive([Cell|T], _Col, [], T) :-
    Cell == '#',
    !.

collect_consecutive([Cell|T], Col, [Col|More], Remainder) :-
    var(Cell),  % Check if the cell is an open cell
    Col1 is Col + 1,
    collect_consecutive(T, Col1, More, Remainder).

/*********************************************************************
 * make_horizontal_slots/3
 * Build slot(horizontal, Len, [(Row,Col),...]) for runs >= 2.
 *********************************************************************/
make_horizontal_slots([], _Row, []).
make_horizontal_slots([RunCols|T], Row, [slot(horizontal,Len,Coords)|Slots]) :-
    length(RunCols, Len),
    Len >= 2,
    findall((Row,Col), member(Col, RunCols), Coords),
    make_horizontal_slots(T, Row, Slots).
make_horizontal_slots([RunCols|T], Row, Slots) :-
    length(RunCols, Len),
    Len < 2,  % skip single-letter run
    make_horizontal_slots(T, Row, Slots).

/*********************************************************************
 * extract_vertical_slots/2
 * Transpose grid => each column becomes a row => reuse horizontal code.
 *********************************************************************/
extract_vertical_slots(Puzzle, VSlots) :-
    transpose(Puzzle, TGrid),
    extract_horizontal_slots(TGrid, TempHSlots),
    fix_transposed_coords(TempHSlots, VSlots).

fix_transposed_coords([], []).
fix_transposed_coords([slot(horizontal,Len,TransCoords)|T1],
                      [slot(vertical,Len,OrigCoords)|T2]) :-
    maplist(swap_rc, TransCoords, OrigCoords),
    fix_transposed_coords(T1, T2).

swap_rc((R,C), (C,R)).

/*********************************************************************
 * fill_slots/3
 * Try to assign each Slot a word from WordList. If none fits, fail.
 *********************************************************************/
fill_slots([], _Puzzle, _WordList) :-
    writeln('All slots filled successfully.').

fill_slots([Slot|OtherSlots], Puzzle, WordList) :-
    writeln(['Filling slot:', Slot]),
    try_words_in_slot(Slot, Puzzle, WordList),
    writeln(['Slot filled:', Slot]),
    fill_slots(OtherSlots, Puzzle, WordList).

/*********************************************************************
 * try_words_in_slot/3
 * Attempt each Word in WordList; if slot_matches_word => unify puzzle.
 *********************************************************************/
try_words_in_slot(Slot, _Puzzle, []) :-
    writeln(['No more words for slot', Slot]),
    fail.
try_words_in_slot(Slot, Puzzle, [Word|Rest]) :-
    writeln(['Trying word', Word, 'in slot', Slot]),
    (   slot_matches_word(Puzzle, Slot, Word)
    ->  writeln(['Success check for', Word]),
        place_word_in_slot(Puzzle, Slot, Word),
        writeln(['Placed word', Word, 'in slot', Slot])
    ;   writeln(['Failed check for', Word]), fail
    )
    ;   try_words_in_slot(Slot, Puzzle, Rest).

/*********************************************************************
 * slot_matches_word/3
 * Checks length and conflicts with known puzzle letters.
 *********************************************************************/
slot_matches_word(Puzzle, slot(_Orient, Length, Coords), Word) :-
    writeln(['Checking if word matches slot:', Word, 'Slot coordinates:', Coords]),
    length(Word, Length),
    writeln(['Word length matches slot length:', Length]),
    check_letters_match(Puzzle, Coords, Word).

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
 * Test Cases using SWI-Prolog's plunit
 * To run the tests, use the command: ?- run_tests.
    * This will execute all the tests defined below.
 *********************************************************************/

:- begin_tests(puzzle_solution).

% Test Case 1: example given
test(example,[nondet]) :-
    Puzzle = [['#', h, '#'], [_, _, _], ['#', _, '#']],
    WordList = [[h, a, t], [b, a, g]],
    puzzle_solution(Puzzle, WordList),
    Puzzle = [['#', h, '#'], ['b', a, 'g'], ['#', t, '#']].

% Test Case 2: testing small puzzle
test(small_puzzle,[nondet]) :-
    Puzzle = [
        [ _,  '#', _ ],
        [ _,   _,  _ ],
        [ '#', _, '#' ]
    ],
    WordList = [
        [c,a,t],
        [b,a,g],
        [i,t],
        [i,f],
        [e,c],
        [a,r]
    ],
    puzzle_solution(Puzzle, WordList),
    Puzzle = [[e, '#', i], [c, a, t], ['#', r, '#']].

% Test Case on wikipedia
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

