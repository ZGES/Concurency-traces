%%%-------------------------------------------------------------------
%%% @author Piotr Pasternak
%%% @copyright (C) 2019
%%% @doc
%%% Serwer, który dla zadanego alfabetu, zestawu traznzakcji i ciągu ich wykonania wyznacza zbiory (nie)zależności, postać normalną Foaty, graf Diekerta i postać normalną
%%% Foaty na podstawie grafu Dikerta. Serwer kożysta z funkcji zawartych w module trace
%%% @end
%%%-------------------------------------------------------------------
-module(trace_server).
-author("Piotr Pasternak").

%|----------------------------------------------------------------------|
%|                           SERVER API                                 |
%|----------------------------------------------------------------------|
-export([start/0, stop/0, getInput/3, dependencySet/0, independencySet/0, fnf/0, diekertGraph/0, fnfFromDiekert/0, computeAll/3, help/0]).

% Funkcja rozpoczyna działanie serwera, należy uruchomić na początku
start() ->
  case lists:member(traceServer, registered()) of
    true ->
      serverIsRunning;
    false ->
      register(traceServer, spawn(fun() -> init() end))
  end.

% Funkcja konczy działanie serwera
stop() ->
  case lists:member(traceServer, registered()) of
    true ->
      traceServer ! terminate;
    false ->
      serverIsNotRunning
  end.

% Funkcja inicijalizująca zmienną globalną
init() ->
  loop([{},[],[],[],[],[]]).

% Ciało i dusza serwera, w reakcji na wiadomość wysłaną na jego PID, serwer wykonuje jedną z poniższych akcji
loop(DataSet) ->
  receive
    {Pid, getInput, Alphabet, TransactionSet, Word} ->
      Status = trace:getInput(Alphabet, TransactionSet, Word),
      case is_tuple(Status) of
        true  -> Pid ! {getInput, dataSaved},
          loop([Status,[],[],[],[],[]]);

        false -> Pid ! {getInput, Status}
      end,
      loop(DataSet);

    {Pid, dSet} ->
      Input = lists:nth(1,DataSet),
      {_, TransactionSet, _} = Input,
      DSet = trace:computeDependencySet(TransactionSet),
      Pid ! {dSet, DSet},
      loop([Input, DSet, [], [], [], []]);

    {Pid, iSet} ->
      Input = lists:nth(1,DataSet),
      {Alphabet, _, _} = Input,
      DependencySet = lists:nth(2,DataSet),
      ISet = trace:computeIndependencySet(Alphabet, DependencySet),
      Pid ! {iSet, ISet},
      loop([Input, DependencySet, ISet, [], [], []]);

    {Pid, fnf} ->
      Input = lists:nth(1,DataSet),
      {_, _, Word} = Input,
      IndependencySet = lists:nth(3,DataSet),
      FNF = trace:fnf(Word, IndependencySet),
      Pid ! {fnf, FNF},
      loop([Input, lists:nth(2,DataSet), IndependencySet, FNF, [], []]);

    {Pid, diekert} ->
      FNF = lists:nth(4, DataSet),
      Diekert = trace:diekertGraph(FNF),
      Pid ! {diekert, Diekert},
      loop([lists:nth(1, DataSet), lists:nth(2,DataSet), lists:nth(3, DataSet), FNF, Diekert, []]);

    {Pid, fnf2} ->
      Diekert = lists:nth(5, DataSet),
      FNF2 = trace:fnfFromDG(Diekert),
      Pid ! {fnf2, FNF2},
      loop([lists:nth(1, DataSet), lists:nth(2,DataSet), lists:nth(3, DataSet), lists:nth(4, DataSet), Diekert, FNF2]);

    terminate ->
      exit(normal)
  end.

%|----------------------------------------------------------------------|
%|                           CLIENT API                                 |
%|----------------------------------------------------------------------|

% Tymi funkcjami my jako użytkownik komunikujemy się z serwerem (funkcje wysyłają komunikat do serwera, a następnie odbierają wyniki i je wyświetlają)
% POSZCZEGÓLNE KROKI NALEŻY WYKONYWAĆ W KOLEJNOŚCI W JAKIEJ UMIESZCZONE SĄ FUNKCJE OD getInput() DO fnfFromDiekert(), INACZEJ SERWER WYŁĄCZY SIĘ
%                                   W RAZIE PROBLEMÓW MOŻNA SKORZYSTAĆ Z FUNKCJI help().

% Pobranie danych wejściowych i wysłanie ich do zmiennej globalnej serwera
getInput(Alphabet, TransactionSet, Word) ->
  case lists:member(traceServer, registered()) of
    true ->
      traceServer ! {self(), getInput, Alphabet, TransactionSet, Word},
      receive
        {getInput, Status} -> Status
      after 2000 ->
        timeout
      end;
    false ->
      serverIsNotRunning
  end.

% Na podstwaie pobranych danych oblicza zbiór zależności
dependencySet() ->
  case lists:member(traceServer, registered()) of
    true ->
      traceServer ! {self(), dSet},
      receive
        {dSet, DSet} -> {'dependency set',DSet}
      after 2000 ->
        timeout
      end;
    false ->
      serverIsNotRunning
  end.

% Na podstwaie pobranych danych i zbioru zależności oblicza zbiór niezależności
independencySet() ->
  case lists:member(traceServer, registered()) of
    true ->
      traceServer ! {self(), iSet},
      receive
        {iSet, ISet} -> {'independency set', ISet}
      after 2000 ->
        timeout
      end;
    false ->
      serverIsNotRunning
  end.

% Na podstwaie pobranych danych i zbioru niezależności oblicza postać normalną Foaty
fnf() ->
  case lists:member(traceServer, registered()) of
    true ->
      traceServer ! {self(), fnf},
      receive
        {fnf, FNF} -> {'Foata Normal Form', FNF}
      after 2000 ->
        timeout
      end;
    false ->
      serverIsNotRunning
  end.

% Na podstwaie postaci normalnej Foaty wypisuje graf Diekerta w formacie DOT
diekertGraph() ->
  case lists:member(traceServer, registered()) of
    true ->
      traceServer ! {self(), diekert},
      receive
        {diekert, Diekert} -> io:format("~s", [Diekert])
      after 2000 ->
        timeout
      end;
    false ->
      serverIsNotRunning
  end.

% Na podstawie napisu w formacie DOT wyznacza postać normalną Foaty
fnfFromDiekert() ->
  case lists:member(traceServer, registered()) of
    true ->
      traceServer ! {self(), fnf2},
      receive
        {fnf2, FNF} -> {'Foata Normal Form v2', FNF}
      after 2000 ->
        timeout
      end;
    false ->
      serverIsNotRunning
  end.

% Wykonanie wszystkich funkcji programu za jednym razem (uwaga: graf Diekerta jest wypisywany jako pierwszy ze względu na użycie formatowania, jego miejsce w ciągu wywołań
% jest w miejscu wypisania ok
computeAll(Alphabet, TransactionSet, Word) ->
  [getInput(Alphabet, TransactionSet, Word),
  dependencySet(),
  independencySet(),
  fnf(),
  diekertGraph(),
  fnfFromDiekert()].

% Pomoc - wymienienie możliwości obliczeń, format danych wejściowych, kolejność funkcji w sposobie krokowym
help() ->
  Text = "\n                     WITAM W POMOCY!!!\nProgram mozna wykonywac na dwa sposoby:\n1)Wszystko za jednym razem (computeAll/3)\n2)Krokowo\n\nFORMAT DANYCH WEJSCIOWYCH
Dane wejsciowe podajemy podczas wywolania funkcji getInput/3 i computeAll/3:\n  Pierwszy argument to lista ze znakami alfabetu: [a1,a2,a3,a4,...,an], gdzie ai to liczba lub atom(pojedyncze slowo lub kilka zawartych w ''), np.
[1,2,3], [a,b,c,d], [jeden, dwa, trzy], ['uwielbiam TW', 'oznaczenia operacji'], [1,a,'dwa slowa'];
  Drugi argument to lista krotek, gdzie pierwsza to symbol alfabetu, a druga to napis reprezentujacy operacje {ai,""x=x+y""}, np.
[{a,""x=20y+z""},{b,""y=y-z""},{c,""z=2x""}];\n  Trzeci argument to lista znakow alfabetu reprezentujaca ciag operacji: [a1,a1,a2,a5,...,a10,a1], np.
[a,b,b,a,c,d,d,a,c,b].\n\nSPOSOB KROKOWY:\n1)trace_server:getInput/3\n2)trace_server:dependencySet/0\n3)trace_server:independencySet/0
4)trace_server:fnf/0\n5)trace_server:diekertGraph/0\n6)trace_server:fnfFromDiekert/0\n
CO WYPISUJE FUNKCJA computeALL/3 ?\nFunkcja ta wypisuje wszystkie wyznaczane rzeczy w kolejnosci takiej jak przy wykonaniu krokowym, z ta roznica, ze na samym poczatku, ze
wzgledu na koniecznosc formatowania wypisany jest graf Diekerta. Na jego miejsce w ciagu wykonan wypisane jest ok, wiec wlasciwa kolejnosc to 5),1)-4),ok,6).\n
                            MAM NADZIEJE ZE CHOCIAZ TROCHE POMOGLO TO W OBSLUDZE PROGRAMU :)",
  io:format("~s", [Text]).