# Obsługa modułów trace.erl i trace_server.erl

Moduły zostały napisane dla Erlanga w wersji 21.3 ale powinny być wstecznie kompatybilne do kilku wersji w tył.

Aby uruchomić programy należy przejść do katalogu w którym się znajdują. Następnie w tym katalogu uruchomić wirtualną maszynę Erlanga poleceniem erl.  Moduły kompilujemy poleceniem c(nazwa modułu) => c(trace) i c(trace_server).

Moduł trace.erl zawiera kod wykonujący główne obliczenia, opatrzony komentarzami.

Moduł trace_server.erl służy do komunikacji z użytkownikiem, pozwala mu na zadanie poleceń do wykonania i wyświetla wyniki.

Gdy skompilujemy moduły program uruchamiamy wywołując funkcję  trace_server:start().

Program możemy wykonywać krokowo wykonując kolejno funkcje(z przedrostkiem trace_server:) :
1) getInput(Alphabet, TransactionSet, Word) w formacie:
	Alphabet = [a1,a2,..,an] (lista znaków alfabetu),
	TransactionSet = [{a, ”x=x+y”},{b, ”y=z+2y”},…] (lista krotek znaków i ich transakcji),
	Word = [a1,a1,an,..,a3,a4,...]  (ciąg wywołań operacji);
2)dependencySet();
3)independencySet();
4)fnf();
5)diekertGraph();
6)fnfFromDiekert();
albo wywołując funkcję trace_server:computeAll(Alphabet, TransactionSet, Word).

W razie wątpliwości należy wywołać funkcję trace_server:help().

Działanie programu kończymy funkcją trace_server:stop().