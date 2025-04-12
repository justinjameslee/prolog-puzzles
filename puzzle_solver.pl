:- module(puzzle_solver, [puzzle_solution/2]).
:- use_module(library(clpfd), [transpose/2]).
:- use_module(library(plunit)).

/*********************************************************************
 * puzzle_solution/2
 * Puzzle is a list-of-lists, each cell is '#' or a letter or _ (var).
 * WordList is a list of word-lists, like [ [c,a,t], [i,t], ... ].
 *********************************************************************/
puzzle_solution(Puzzle, WordList) :-
    extract_all_slots(Puzzle, UnorderedSlots),
    sort_slots_by_prefilled(Puzzle, UnorderedSlots, Slots),
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
    Cell \== '#', % Check if the cell is an open cell
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
    Cell \== '#', % Check if the cell is an open cell
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
 * Sort slots by how many cells are already filled with letters
 * in the puzzle.  More pre-filled => fill earlier => ensures
 * partial constraints are satisfied first.
 *********************************************************************/
sort_slots_by_prefilled(_Puzzle, [], []).
sort_slots_by_prefilled(Puzzle, [Slot|Others], SortedSlots) :-
    slot_prefilled_count(Puzzle, Slot, Count),
    sort_slots_by_prefilled(Puzzle, Others, Rest),
    insert_by_count(Slot-Count, Rest, SortedSlots).

/*********************************************************************
 * slot_prefilled_count/3
 * Count how many cells in this slot are already a letter (var(Cell) => 0).
 *********************************************************************/
slot_prefilled_count(_Puzzle, slot(_Orient, _Len, []), 0) :-
    writeln('Base case reached: Count = 0').

slot_prefilled_count(Puzzle, slot(Orient, Len, [(R, C) | Coords]), Count) :-
    nth0(R, Puzzle, Row),
    nth0(C, Row, Cell),
    (   var(Cell) -> ThisOne = 0  % Cell is unbound
    ;   Cell == '#' -> ThisOne = 0  % Cell is a block
    ;   ThisOne = 1  % Cell is prefilled with a letter
    ),
    writeln(['Processing cell:', (R, C), 'Cell:', Cell, 'ThisOne:', ThisOne]),
    slot_prefilled_count(Puzzle, slot(Orient, Len, Coords), Rest),  % Recursive call
    writeln(['Rest count:', Rest]),
    Count is ThisOne + Rest,  % Compute total count
    writeln(['Total count:', Count]).

/*********************************************************************
 * insert_by_count/3
 * Insert Slot-Count into sorted list, so bigger counts come first.
 *********************************************************************/
insert_by_count(SC, [], [SC]).
insert_by_count(Slot-Count, [OtherSlot-OC|Rest], [Slot-Count,OtherSlot-OC|Rest]) :-
    Count >= OC, !.
insert_by_count(SC, [X|Rest], [X|NewRest]) :-
    insert_by_count(SC, Rest, NewRest).

/*********************************************************************
 * Now we fill the sorted slots from "most prefilled" to "least prefilled".
 *********************************************************************/
fill_slots([], _Puzzle, _WordList) :-
    writeln('All slots filled successfully.').

fill_slots([Slot-_|OtherSlots], Puzzle, WordList) :-
    % ^ note that we store (slot(Orient,Len,Coords) - PrefilledCount)
    % so we just pick the first part for the actual slot
    Slot = slot(_,_,_),
    writeln(['Filling slot:', Slot]),
    select_a_word_and_fill(Slot, Puzzle, WordList, NewWordList),
    fill_slots(OtherSlots, Puzzle, NewWordList).

/*********************************************************************
 * select_a_word_and_fill/4
 * We try each Word from WordList, but upon success, remove that Word
 * for future slots so it can't be reused.
 *********************************************************************/
select_a_word_and_fill(Slot, Puzzle, [Word|RestWords], Rest) :-
    writeln(['Trying word', Word, 'in slot', Slot]),
    (   slot_matches_word(Puzzle, Slot, Word)
    ->  % If it fits, place it, and *this* Word is consumed
        writeln(['Success check for', Word]),
        place_word_in_slot(Puzzle, Slot, Word),
        writeln(['Placed word', Word, 'in slot', Slot]),
        Rest = RestWords  % remove Word from the list for future
    ;   writeln(['Failed check for', Word]),
        fail
    )
    ;   % If fail, backtrack to try next Word in the list
        select_a_word_and_fill(Slot, Puzzle, RestWords, Rest).

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
 *********************************************************************/

:- begin_tests(puzzle_solution).

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

test(small_puzzle,[nondet]) :-
    Puzzle = [
        [ _,  '#', _ ],
        [ _,   _,  _ ],
        [ '#', _, '#' ]
    ],
    WordList = [
        [c, a, t],
        [b, a, g],
        [i, t],
        [i, f],
        [e, c],
        [a, r]
    ],
    puzzle_solution(Puzzle, WordList),
    ExpectedPuzzle = [
        [ e, '#', i ] , 
        [ c,  a,  t ], 
        ['#', r, '#']
    ],
    assertion(Puzzle == ExpectedPuzzle).

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
