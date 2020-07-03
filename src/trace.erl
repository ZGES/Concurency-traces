%%%-------------------------------------------------------------------
%%% @author Piotr Pasternak
%%% @copyright (C) 2019
%%% @doc
%%% Zestaw funkcj słuzących do wyznaczania zbioru (nie)zależności, FNF i grafu Diekerta dla ciągu zależnych między sobą operacji
%%% @end
%%%-------------------------------------------------------------------
-module(trace).
-author("Piotr Pasternak").

%% API
-export([getInput/3, computeDependencySet/1, computeIndependencySet/2, fnf/2, diekertGraph/1, fnfFromDG/1]).

%  Funkcja pobiera wejście w formacie: getInput([a,b,c,d], [{a,"x = x + y"}, {b, "y = 2z - x}, ...], [b,a,c,d,d,d,a]), lista znaków alfabetu; lista krotek z transakcjami,
%  gdzie pierwszym elementem krotki jest symbol alfabetu, a drugim string opisujący transakcję; lista reprezentująca słowo. Zwracana jest krotka będąca zestawem danych dla serwera
getInput(Alphabet, TransactionSet, Word) when is_list(Alphabet) and is_list(TransactionSet) and is_list(Word)->
  {Alphabet, TransactionSet, Word};
getInput(_,_,_) ->
  inputError.

%----------------------------------------------------------------------------------------------------------------------------

% Funkcja tworzy zbiór zależności na podstawie zbioru transakcji (w wyniku przyjmuję za domyślną symetrię zbioru zależności, by uniknąć redundancji wyników)
computeDependencySet(TransactionSet) ->
  computeDependencySetHelper(TransactionSet, []).

% Funkcja pomocnicza, wydobywa znak alfabetu, lewą i prawą stronę transakcji (by uniknąć redundancji w findDependency() robimy to tutaj), a następnie szuka zależności pomiędzy
% obecną transakcją i wszystkimi "późniejszymi". Zwraca nam zbiór zależności.
computeDependencySetHelper([], DependencySet) ->
  % Odwracamy listę, ponieważ ze względu na wydajność, elementy do listy dodajemy na jej początku
  lists:reverse(DependencySet);
computeDependencySetHelper([H|T], DependencySet) ->
  {CurrentID, [F|Others]} = H,
  % Przekzauję cały TransactionSet ([H|T] zamieas samego T) jako 4. argument, by nie martwić się zwrotnością
  computeDependencySetHelper(T, findDependency(CurrentID, F, Others, [H|T], DependencySet)).

% Funkcja wykonująca właściwe poszukiwanie zależności
findDependency(_, _, _, [], DependencySet) ->
  DependencySet;
findDependency(CurrentID, F, Others, [H|T], DependencySet) ->
  % Wydobycie id transakcji i jej ciała
  {TransactionID, Transaction} = H,
  % Funkcja lists:member zwraca True, jeśli pierwszy argument znajduje się w liście podanej jako drugi argument
  case lists:member(F, Transaction) of
    true -> findDependency(CurrentID, F, Others, T, [{CurrentID, TransactionID}| DependencySet]);

    % Jeśli nie, to sprawdzam, czy po prawej stronie obecnej transakcji występuje lewy element nowej transakcji
    _ -> [NewF|_] = Transaction,
      case lists:member(NewF, Others) of
        % Jak tak, to dodajemy do zbioru
        true -> findDependency(CurrentID, F, Others, T, [{CurrentID, TransactionID}| DependencySet]);

        % A jak nie, to przechodzimy do kolejnej transakcji, nie wprowadzając zmian w zbiorze
        _ -> findDependency(CurrentID, F, Others, T, DependencySet)
      end
  end.

%----------------------------------------------------------------------------------------------------------------------------

% Funkcja wyznaczająca zbiór niezależności. Lists:subtract usuwa z listy, bedącej pierwszym argumentem, pierwsze wystąpienie elementu z drugiej listy, dla każdego wystapienia
% elementu w drugiej liście.
computeIndependencySet(Alphabet, DependencySet) ->
  lists:subtract(fullSet(Alphabet, []), DependencySet).

% Funkcja tworzy iloczyn kartezjański zbioru ze sobą, jednak pomija symetryczne pary, jeśli elementy pary są różne
fullSet([], List) ->
  % Listę odwracamy, dla czytelniejszego wyniku. Lists:flatten wypłaszcza listę list do pojedynczej listy
  lists:reverse(lists:flatten(List));
fullSet([H|T], List) ->
  % Wyciągamy element z listy, i tworzymy listę długości listy z elementami niewczesniejszymi, zawierająca tylko ten element (lists:duplicate);
  % na otrzymanej liście i liście wejściowej wykonujemy iloczyn kartezjanski (lists:zip) i tak otrzymujemy wszystkie pary litery alfabetu z wszystkimi niepóżniejszymi;
  % otrzymaną listę odwracamy dla ładniejszego wyniku i dołaczamy na początek listy wynikowej
  fullSet(T,[lists:reverse(lists:zip(lists:duplicate(length([H|T]), H), [H|T]))|List]).

%----------------------------------------------------------------------------------------------------------------------------

% Funkcja wyznaczająca postać normalą Foaty; format wyjścia to lista list, gdzie wewnetrzna lista reprezentuje pojedynczą klasę
fnf(Word, IndependencySet) ->
  fnfHelper(Word, IndependencySet, [], []).

% Funkcja pomocnicza, główne obliczenia
% Jeśli skończyło nam się słowo, to do postaci normalnej dodajemy aktualnie rozważaną klasę; lists:reverse() dla ładniejszego wyniku
fnfHelper([], _, Class, FNFList) ->
  lists:reverse([lists:reverse(Class)|FNFList]);
fnfHelper([H|T], IndependencySet, Class, FNFList) ->
  % Badamy, czy obecny znak w słowie zależy od któregoś ze znaków zawartych w obecnie badanej klasie
  case areDependent([{X,H} || X <- Class], IndependencySet) of
    % Jesli tak, to tworzymy nową klasę, a starą wrzucamy do listy
     true -> fnfHelper(T, IndependencySet, [H], [lists:reverse(Class)|FNFList]);

    % Wpp rozwijamy naszą klasę
    _ -> fnfHelper(T, IndependencySet, [H|Class], FNFList)
  end.

% Funkcja która sprawdza, czy dwa elementy zależą od siebie (badamy to na niezależności, ponieważ w przykładach zbiór ten jest krótszy, ale mozna funkcję zmodyfikować, by
% sprawdzała zbiór zależności, tylko trzeba odwrócić warunki logiczne (funkcja areDependent2 użyta podczas wyznaczania FNF z grafu)
areDependent([], _) ->
  false;
areDependent([H|T], IndependencySet) ->
  {A, B} = H,
  case lists:member(H, IndependencySet) of
    true -> areDependent(T, IndependencySet);

    _ -> case lists:member({B, A}, IndependencySet) of
           true -> areDependent(T, IndependencySet);

           _ -> true
         end
  end.

%----------------------------------------------------------------------------------------------------------------------------

% Funkcja dostarcza graf w formacie DOT
diekertGraph(FNF) ->
  diekertGraphHelper(matchFNFtoInt(FNF, [], 1), matchFNFtoInt(FNF, [], 1), "digraph g{\n").

% Właściwe mapowanie FNF na DOT
diekertGraphHelper(_, [], Dot) ->
  lists:concat([Dot, "}"]);
diekertGraphHelper([F,S|T], MatchList, Dot) ->
  diekertGraphHelper([S|T], MatchList, addToDOT(Dot, [{X,Y} || {_,X} <- F, {_,Y} <- S]));
diekertGraphHelper(List, [H|T], Dot) ->
  diekertGraphHelper(List, T, addLabelsToDOT(Dot, H)).

% Funkcja numeruje wystąpienia czynności, by móc narysować graf Diekerta
matchFNFtoInt([], ModifiedList, _)->
  lists:reverse(ModifiedList);
matchFNFtoInt([H|T], ModifiedList, Counter) ->
  matchFNFtoInt(T, [lists:zip(H, lists:seq(Counter,Counter+length(H)-1))|ModifiedList], Counter+length(H)).

% Funkcja służąca do dodania do napisu reprezentującego format DOT grafu zależności między kolejnymi czynnościami
addToDOT(Dot, []) ->
  Dot;
addToDOT(Dot, [H|T]) ->
  {Val1, Val2} = H,
  addToDOT(lists:concat([Dot, Val1, " -> ", Val2, "\n"]), T).

% Funkcja przypisuje numerom reprezentującym węzły odpowiadające im znaki z alfabetu
addLabelsToDOT(Dot, []) ->
  Dot;
addLabelsToDOT(Dot, [H|T]) ->
  {AlphChar, Value} = H,
  addLabelsToDOT(lists:concat([Dot, Value, "[ label=", AlphChar, " ]\n"]), T).

%----------------------------------------------------------------------------------------------------------------------------

% Funkcja wyznacza FNF na podstawie grafu Dikerta w formacie DOT
fnfFromDG(Dot) ->
  % '62' w list member to kod ascii '>'; tutaj dzielimy napis po znaku nowej linii i tworzymy listę napisów, usuwamy z niej element ostatni i pierwszy, ponieważ nie są
  % potrzebne do wyznaczania FNF, a następnie dzielimy napisy na dwie osobne listy: 1. z zależnosciami wierzchołków, 2. z przypisaniem liczbom symboli z alfabetu
  {DepList, MatchList} = lists:partition(fun(X) -> lists:member(62, X) end, lists:delete("digraph g{",lists:droplast(string:split(Dot, "\n", all)))),
  computeFNFfromDG(getDependency(DepList, []), getDependency(DepList, []), [], [], [], getMatch(MatchList, []), getDependency(DepList, [])).

% Funkcja wykonuje właściwe obliczenia FNF, najpierw wykonuje sortowanie topologiczne i ustala przynależność do klas Foaty, a na koniec mapuje wierzchołki na litery alfabetu
% Dokładniej: sprawdzam, czy wierzchołek zależy od innych, jak nie, to dodaję go do klasy, o ile jeszcze go w niej nie ma, jeśli zależy, to dodaję go do listy którą przetworzę
% w kolejnej iteracji. Gdy lista staje się pusta to dodaję klasę do listy FNF; jeśli zostały mi wierzchołki do rozpatrzenia na później to powtarzam proces, na danym poziomie
% na pewno będzie conajmniej jeden niezależny. Gdy nie zostały mi wierzchołki do rozpatrzenia, to dodaję wirzchołki z przedostatiej klasy, wyznaczam ostatnia klasę i ją dodaję
% a na koniec mapuję wierzchołki na znaki alfabetu
computeFNFfromDG([], _, FNFList, Class, [], MatchList, DList) ->
  AlmostFNFList = [Class|FNFList],
  map(lists:reverse([getLastClass(DList, DList, [])|AlmostFNFList]), MatchList, []);
computeFNFfromDG([], _, FNFList, Class, Wastes, MatchList, DList) ->
  computeFNFfromDG(Wastes, Wastes, [Class|FNFList], [], [], MatchList, DList);
computeFNFfromDG([H|T], DepList, FNFList, Class, Wastes, MatchList, DList) ->
  {Num, _} = H,
  case isDependingOn(Num, DepList) of
    true -> computeFNFfromDG(T, DepList, FNFList, Class, [H|Wastes], MatchList, DList);

    _ -> case lists:member(Num, Class) of
           false -> computeFNFfromDG(T, DepList, FNFList, [Num|Class], Wastes, MatchList, DList);

           _ -> computeFNFfromDG(T, DepList, FNFList, Class, Wastes, MatchList, DList)
         end
  end.

% Funkcja mapująca wierzchołki z FNFList na znaki alfabetu
map([], _, MappedList) ->
  lists:reverse(MappedList);
map([H|T], MatchList, MappedList) ->
  map(T, MatchList, [match(H, MatchList, MatchList, [])|MappedList]).

% Funkcja pomocnicza map, mapuje wierzchołki dla poszczególnych podlist FNFList
match([], _, _, MappedList) ->
  lists:reverse(MappedList);
match([H|T], [MatchH|MatchT], MatchList, MappedList) ->
  {Val, Alph} = MatchH,
  case H == Val of
    true-> match(T, MatchList, MatchList, [Alph|MappedList]);

    _ -> match([H|T], MatchT, MatchList, MappedList)
  end.

% Funkcja sprawdza, czy wierzchołek znajduje się na rozpatrywanym poziomie (czy należy do klasy Foaty) 
isDependingOn(_, []) ->
  false;
isDependingOn(Num, [H|T]) ->
  {_, Depending} = H,
  case Num == Depending of
     true -> true;
    
    _ -> isDependingOn(Num, T)
  end.

% Funkcja wyznacza ostatnią z klas Foaty
getLastClass([], _, Class) ->
  lists:reverse(Class);
getLastClass([H|T], DepList, Class) ->
  {_, Vertex} = H,
  case hasNoDependent(Vertex, DepList) of
    true -> case lists:member(Vertex, Class) of
              true -> getLastClass(T, DepList, Class);

              _ -> getLastClass(T, DepList, [Vertex|Class])
            end;

    _ -> getLastClass(T, DepList, Class)
  end.

% Funkcja sprawdza, czy wierchołek poprzedza inny, jak nie zwraca true, flase wpp.
hasNoDependent(_, []) ->
  true;
hasNoDependent(Vertex, [H|T]) ->
  {LeftSide, _} = H,
  case Vertex == LeftSide of
    true -> false;

    _ -> hasNoDependent(Vertex, T)
  end.

%-----------------Ponizsze funkcje służą do obsługi Stringów---------------------------

% {Vertex, DependencyVertex}
getDependency([], DepList) ->
  lists:reverse(DepList);
getDependency([H|T], DepList) ->
  getDependency(T, [extractInts(H, [])|DepList]).

extractInts([], [H|T]) ->
  list_to_tuple(lists:reverse([list_to_integer(lists:reverse(integer_to_list(H)))|T]));
extractInts(String, List) ->
  {Val, _} = string:to_integer(String),
  case length(List) < 2 of
      true -> extractInts(lists:reverse(String), [Val|List]);

      _ -> extractInts([], List)
  end.

% {Val, Alph}
getMatch([], MatchList) ->
  lists:reverse(MatchList);
getMatch([H|T], MatchList) ->
  getMatch(T, [extractCouple(H)|MatchList]).

extractCouple(String) ->
  {Val, _} = string:to_integer(String),
  extractCouple(lists:reverse(lists:droplast(lists:droplast(String))), [Val]).

extractCouple([], [H|T]) ->
  case is_integer(H) of
    true -> list_to_tuple(lists:reverse([list_to_integer(lists:reverse(integer_to_list(H)))|T]));

    _ -> list_to_tuple(lists:reverse([list_to_atom(lists:reverse(atom_to_list(H)))|T]))
  end;
extractCouple(String, List) ->
  Elem = string:slice(String, 0, 1),
  {Alph, _} = string:to_integer(Elem),
  case is_integer(Alph) of
    true -> extractCouple([], [Alph|List]);

    _ -> extractCouple([], [list_to_atom(Elem)|List])
  end.
