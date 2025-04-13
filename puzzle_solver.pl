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
 * We do:
 *  (1) Extract all slots.
 *  (2) Sort them by how many letters are already prefilled.
 *  (3) Fill each slot, removing each used word.
 *  (4) We fail if leftover words remain after filling all slots.
 *
 * Key improvement: skip words that do not match the slot's length
 *                 to prune the search drastically.
 *********************************************************************/
puzzle_solution(Puzzle, WordList) :-
    precompute_letter_frequencies(WordList, LetterFreqs),
    writeln(['Letter frequencies:', LetterFreqs]),
    sort_words_by_heuristic(WordList, LetterFreqs, SortedWordList),
    writeln(['Sorted WordList:', SortedWordList]),
    extract_all_slots(Puzzle, UnorderedSlots),
    sort_slots_by_prefilled(Puzzle, UnorderedSlots, Slots),
    fill_slots(Slots, Puzzle, SortedWordList).

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
    Col1 is Col + 1,
    collect_runs(T, Col1, Runs).

collect_runs([Cell|T], Col, [[Col|MoreCols] | OtherRuns]) :-
    Cell \== '#',  % open cell
    ColPlusOne is Col + 1,
    collect_consecutive(T, ColPlusOne, MoreCols, Remainder),
    length(MoreCols, RunLength),
    NextCol is Col + RunLength + 1,
    collect_runs(Remainder, NextCol, OtherRuns).

collect_consecutive([], _Col, [], []) :- !.
collect_consecutive([Cell|T], _Col, [], T) :-
    Cell == '#', 
    !.
collect_consecutive([Cell|T], Col, [Col|More], Remainder) :-
    Cell \== '#',
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

/*********************************************************************
 * extract_vertical_slots/2
 * Transpose puzzle => each column => a "row" => reuse horizontal code.
 *********************************************************************/
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
 * sort_slots_by_prefilled/3
 * For each slot, count how many puzzle cells are already letters.
 * Then sort by that count in descending order (more prefilled => earlier).
 *********************************************************************/
sort_slots_by_prefilled(_, [], []).
sort_slots_by_prefilled(Puzzle, [Slot|Others], SortedSlots) :-
    slot_prefilled_count(Puzzle, Slot, PrefilledCount),
    Slot = slot(_, Length, _),
    sort_slots_by_prefilled(Puzzle, Others, Rest),
    insert_by_count(Slot-(PrefilledCount, Length), Rest, SortedSlots).

slot_prefilled_count(_Puzzle, slot(_Orient,_Len,[]), 0).
slot_prefilled_count(Puzzle, slot(Orient, Len, [(R,C)|Coords]), Count) :-
    nth0(R, Puzzle, Row),
    nth0(C, Row, Cell),
    (   var(Cell)
    ->  ThisOne = 0
    ;   Cell == '#'
    ->  ThisOne = 0
    ;   ThisOne = 1
    ),
    slot_prefilled_count(Puzzle, slot(Orient,Len,Coords), Rest),
    Count is ThisOne + Rest.

insert_by_count(SC, [], [SC]).
insert_by_count(SlotA-(CountA, LengthA), [SlotB-(CountB, LengthB)|Rest], [SlotA-(CountA, LengthA), SlotB-(CountB, LengthB)|Rest]) :-
    (CountA > CountB ; (CountA == CountB, LengthA >= LengthB)), !.
insert_by_count(SC, [X|Rest], [X|NewRest]) :-
    insert_by_count(SC, Rest, NewRest).


sort_words_by_heuristic(WordList, LetterFreqs, SortedWordList) :-
    maplist(word_score_with_word(LetterFreqs), WordList, ScoredWords),
    keysort(ScoredWords, SortedScoredWords),
    reverse(SortedScoredWords, DescendingScoredWords),  % Sort descending
    pairs_values(DescendingScoredWords, SortedWordList).

word_score_with_word(LetterFreqs, Word, Score-Word) :-
    word_score(Word, LetterFreqs, Score).

% Precompute letter frequencies for the entire WordList
precompute_letter_frequencies(WordList, LetterFreqs) :-
    flatten(WordList, Letters),  % Flatten all words into a single list of letters
    findall(Letter-Count, (member(Letter, Letters), count_occurrences(Letter, Letters, Count)), Pairs),
    sort(Pairs, LetterFreqs).  % Remove duplicates and sort by letter

% Count occurrences of a letter in a list
count_occurrences(Letter, List, Count) :-
    include(=(Letter), List, Filtered),
    length(Filtered, Count).

word_score(Word, LetterFreqs, Score) :-
    findall(Freq, (member(Letter, Word), member(Letter-Freq, LetterFreqs)), Frequencies),
    sum_list(Frequencies, Score).

/*********************************************************************
 * fill_slots/3
 * Fill each slot exactly once, removing used words from WordList
 * If leftover words remain after last slot => fail.
 *********************************************************************/
fill_slots([], _Puzzle, WordList) :-
    writeln(['Final WordList:', WordList]),
    ( WordList == [] ->
         writeln('All slots filled successfully.'), !
      ;  writeln('No more slots, but leftover words remain => fail'),
         fail
    ).

fill_slots([Slot-_|OtherSlots], Puzzle, WordList) :-
    writeln(['Current WordList:', WordList]),
    ( WordList == [] ->
        writeln('Ran out of words before filling all slots => fail'),
        fail
    ; true
    ),
    Slot = slot(_,Len,Coords),
    writeln(['Filling slot:', Slot]),
    select_a_word_and_fill(Slot, Len, Coords, Puzzle, WordList, NewWordList),
    writeln(['WordList after filling slot:', NewWordList]),
    fill_slots(OtherSlots, Puzzle, NewWordList).

/*********************************************************************
 * select_a_word_and_fill/6
 * Try each Word in WordList. On success, remove that Word. Otherwise fail.
 * Also skip words that do not match the slot's length, to prune search.
 *********************************************************************/
select_a_word_and_fill(Slot, SlotLen, Coords, Puzzle, WordList, FinalWordList) :-
    % Temporarily filter words by length
    include(correct_length(SlotLen), WordList, FilteredWords),
    writeln(['Filtered WordList for slot length', SlotLen, ':', FilteredWords]),
    try_words(Slot, Coords, Puzzle, FilteredWords, WordList, FinalWordList).

filter_and_sort_words(SlotLen, WordList, LetterFreqs, SortedWords) :-
    findall(Score-Word,
        (   member(Word, WordList),
            length(Word, SlotLen),  % Filter by length
            word_score(Word, LetterFreqs, Score)  % Compute heuristic score
        ),
        ScoredWords),
    keysort(ScoredWords, SortedScoredWords),
    reverse(SortedScoredWords, DescendingScoredWords),  % Sort descending
    pairs_values(DescendingScoredWords, SortedWords).

correct_length(SlotLen, Word) :-
    length(Word, SlotLen).

try_words(_, _, _, [], _, _) :-
    writeln(['No words left to try for this slot. Backtracking...']),
    fail.  % No words left to try, fail

try_words(Slot, Coords, Puzzle, [Word|RestWords], OriginalWordList, FinalWordList) :-
    writeln(['Trying word', Word, 'in slot', Slot]),
    (   slot_matches_word(Puzzle, Slot, Word)
    ->  writeln(['Success check for', Word]),
        place_word_in_slot(Puzzle, Slot, Word),
        writeln(['Placed word', Word, 'in slot', Slot]),
        % Remove the word from the current WordList
        select(Word, OriginalWordList, UpdatedWordList),
        writeln(['WordList after removal:', UpdatedWordList]),
        FinalWordList = UpdatedWordList
    ;   writeln(['Failed check for', Word]),
        % Pass the current WordList (unchanged) to the next recursive call
        try_words(Slot, Coords, Puzzle, RestWords, OriginalWordList, FinalWordList)
    ).

/*********************************************************************
 * slot_matches_word/3
 * Checks if Word is correct length & no letter conflicts with puzzle.
 * (We've already filtered by length, but let's keep the code.)
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
 * Tests
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
