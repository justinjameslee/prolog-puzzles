:- module(puzzle_solver, [puzzle_solution/2]).
:- use_module(library(clpfd), [transpose/2]).
:- use_module(library(plunit)).

/*********************************************************************
 * puzzle_solution/2
 *
 * The main entry point: 
 *   Puzzle = list-of-lists, each cell either '#', a letter, or var(_).
 *   WordList = list of words, each word is a list of letters.
 *
 * Changes from prior code:
 *  (1) No "sorting by prefilled letters" or slot-length. Instead, we
 *      do a dynamic "most constrained slot first" approach.
 *  (2) We keep a dictionary of words by length => WordDict.
 *      This prevents scanning the entire word list for each slot.
 *
 * Steps:
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
 * Example: If WordList = [[c,a,t],[b,a,g],[i,t],[i,f]]
 *   Then WordDict might be:
 *   [
 *     2 -> [[i,t],[i,f]],
 *     3 -> [[c,a,t],[b,a,g]]
 *   ]
 *
 * Implementation detail:
 *   We'll store it as a list of pairs: [ length-L, list-of-words ].
 *   Then we can look up by length with an auxiliary 'get_words_of_length/3'.
 *********************************************************************/
build_word_dict(WordList, WordDict) :-
    /* Group words by length using an accumulator approach. */
    group_words_by_length(WordList, [], WordDictUnsorted),
    /* Sort the dictionary by length ascending (not crucial, but neat). */
    sort(WordDictUnsorted, WordDict).

/* group_words_by_length/3 does the grouping recursively. */
group_words_by_length([], Acc, Acc).
group_words_by_length([W|Ws], Acc, Out) :-
    length(W, L),
    /* Insert W into the bucket for length L. */
    insert_word_in_dict(L, W, Acc, Acc2),
    group_words_by_length(Ws, Acc2, Out).

/* insert_word_in_dict(+Len, +Word, +DictIn, -DictOut).
   If there's a pair (Len->List), we add Word to that list,
   else we create a new pair. */
insert_word_in_dict(L, W, [], [L-[W]]) :- !.
insert_word_in_dict(L, W, [Len-Words|Rest], [Len-[W|Words]|Rest]) :-
    L == Len, !.
insert_word_in_dict(L, W, [Pair|Rest], [Pair|NewRest]) :-
    insert_word_in_dict(L, W, Rest, NewRest).

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

row_to_slots(RowList, RowIndex, RowSlots) :-
    collect_runs(RowList, 0, Runs),
    make_horizontal_slots(Runs, RowIndex, RowSlots).

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

extract_vertical_slots(Puzzle, VSlots) :-
    transpose(Puzzle, TGrid),
    extract_horizontal_slots(TGrid, TempHSlots),
    fix_transposed_coords(TempHSlots, VSlots).

fix_transposed_coords([], []).
fix_transposed_coords([slot(horizontal,Len,TC)|T],[slot(vertical,Len,OC)|T2]) :-
    maplist(swap_rc, TC, OC),
    fix_transposed_coords(T, T2).

swap_rc((R,C),(C,R)).

/*********************************************************************
 * get_words_of_length(+Dict, +Length, -WordsList)
 * Retrieve the list of words for 'Length' from the dictionary.
 * If none found, WordsList = [].
 *********************************************************************/
get_words_of_length([], _L, []).
get_words_of_length([Len-Words|_], L, Words) :- L == Len, !.
get_words_of_length([_|Rest], L, Words) :-
    get_words_of_length(Rest, L, Words).

/*********************************************************************
 * remove_word_from_dict(+DictIn, +Length, +Word, -DictOut)
 * Remove 'Word' from the dictionary bucket for 'Length'.
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
 * fill_puzzle(+Puzzle, +Slots, +WordDict).
 *
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
 *********************************************************************/
pick_most_constrained_slot(Puzzle, [S|Ss], WordDict, Slot, OtherSlots) :-
    /* Evaluate # of candidates for S. */
    candidates_for_slot(Puzzle, S, WordDict, CandS),
    length(CandS, CountS),
    pick_most_constrained_slot_aux(Puzzle, Ss, WordDict, S, CountS, Slot),
    /* Remove that Slot from the big list => OtherSlots is the rest. */
    delete([S|Ss], Slot, OtherSlots).

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
 *********************************************************************/
candidates_for_slot(Puzzle, slot(_Orient, Len, Coords), WordDict, Candidates) :-
    /* 1) Get all words for 'Len' from dictionary. */
    get_words_of_length(WordDict, Len, WordCandidates),
    writeln(['Word candidates for length', Len, '=>', WordCandidates]),
    /* 2) Filter out invalid candidates. */
    include(validate_candidate(Puzzle, Coords), WordCandidates, Candidates),
    writeln(['Valid candidates after check:', Candidates, 'for slot with coords', Coords]).

validate_candidate(Puzzle, Coords, Word) :-
    validate_cells(Puzzle, Coords, Word).

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
 *********************************************************************/
try_fill_slot_with_candidates(_Puzzle, _Slot, _OtherSlots, _WordDict) :-
    /* No candidates => fail. */
    fail.

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
    /* Undo the partial unify. 
       We'll do it by *not committing* to place_word_in_slot/3 
       but we have to revert the puzzle somehow. 
       Easiest is to rely on Prolog backtracking if place_word_in_slot
       doesn't have a cut. Let's wrap carefully... */

    fail.

/*********************************************************************
 * place_word_in_slot/3
 * Actually unify puzzle cells with Word letters. (No need to revert if we fail
 * because Prolog automatically backtracks these unifications.)
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

:- end_tests(puzzle_solution).
