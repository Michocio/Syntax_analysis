% Języki i Paradygmaty Programowania
% Parser LR(0)
% Projekt Prolog
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Automat reprezentuję jako dwie listy:
% -> Jedna zawiera przejścia i przesunięcia
% -> Druga redukcje i akceptacje
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Podstawowe termy:
% produkcja(Numer, LHS, RHS).
% sytuacja(NumerProdukcji, PozycjaKropki). Pozycja numerowane od 0

% UWAGA: w opisie wielu poniższych procedur pisze "produkcje rozdzielone",
% to skrót myślowy od rozbijania produkcji będących w liście(tak jak są dostarczone),
% do pojedynczych produkcji. prod('A',[[x],[y]]), po "rozbiciu" staje się
% prod('A',[[x]]), prod('A',[[y]]).
% UWAGA2: nt - to skrót od nieterminal.

% moja_gramatyka(+SymbStart, +Produkcje, -Wynik)
% W Wynik produkcje rozdzielone na pojedyncze RHS,
% z dodaną nową produkcją 'Z -> SymbStart#' (potrzebne przy parsowaniu).


moja_gramatyka(SymbStart, Produkcje, Wynik) :-
	transformuj(1, Produkcje, Gramatyka),%Rozdziela produkcje do pojedynczych prod
	my_flatten([prod(0, 'Z', [nt(SymbStart),#]) | Gramatyka], Wynik). %Dodatkowa prod


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Procedury pomocnicze dla operacji na gramatyce.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% produkcjeNumer(+Gramatyka, ?Numer, ?Produkcja)
% W Produkcja, produkcja o numerze Numer, wydobyta z Gramatyka.
produkcjeNumer([], _, []). % Nie znaleziono
produkcjaNumer([prod(Nr, LHS, RHS) | _], Nr, prod(Nr, LHS, RHS)). % Znaleziono
produkcjaNumer([prod(Id, _, _) | Gramatyka], Numer, Wynik) :-
	Id \= Numer,
	produkcjaNumer(Gramatyka, Numer, Wynik). % Dalsze poszukiwania

% produkcjeSymbol(+Gramatyka, +LHS, -Produkcje)
% W Produkcje, wszystkie produkcje z Gramatyka, dla danego LHS.
% prod(-Nr, +LHS, -RHS) - dopasowana do LHS produkcja
produkcjeSymbol([], _, []).  % Nie znaleziono
produkcjeSymbol([prod(Nr, LHS, RHS) | Gramatyka], LHS, [prod(Nr, LHS, RHS) | Wynik]) :-
	produkcjeSymbol(Gramatyka, LHS, Wynik).
produkcjeSymbol([prod(_, X, _) | Gramatyka], LHS, Wynik) :-
	X \= LHS,
	produkcjeSymbol(Gramatyka, LHS, Wynik). % Dalsze poszukiwania

% produkcje_do_sytuacji(+Produkcje, -Sytuacje)
% W Sytuacje, Produkcje zamienione na sytuacje z kropką na pozycji 0.
produkcje_do_sytuacji([],[]).
produkcje_do_sytuacji([prod(Nr, _, _) | Ogon], [sytuacja(Nr, 0) | Sytuacje]) :-
	produkcje_do_sytuacji(Ogon, Sytuacje).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% transformuj(+Nr, +Gramatyka, -Wynik).
% W Wynik produkcje, pojedyncze dla pojedynczego LHS(czyli "rozbite").
transformuj(_, [], []).
transformuj(Numer, [prod(Symb, LHSLista) | Produkcje], [Wynik | Reszta]) :-
	petla_prod(Numer, Symb, LHSLista, Wynik, Index),
	transformuj(Index, Produkcje, Reszta).


% petla_prod(+Nr, +Symbol, +Gramatyka, -Produkcje, -Index).
% W Wynik produkcje RHS, pojedyncze dla pojedynczego LHS.
% W Index pierwszy wolny numer dla produkcji.
petla_prod(Num, _, [], [], Num).
petla_prod(Nr, Symbol, [Prod | Produkcje], [prod(Nr, Symbol, Prod) | Reszta], Index) :-
	Ktory is Nr + 1,
	petla_prod(Ktory, Symbol, Produkcje, Reszta, Index).%Kolejny obieg pętli


% domkniecie(+Gramatyka, +Sytuacje, -StaraLista, -NowaLista)
% NowaLista jest zbiorem sytuacji otrzymanych z Sytuacji, poprzez
% reguły dopełniania(pominę opis - wiadomo z teorii o parserach LR).
% StaraLista - akumulator
domkniecie(_, [], S, S).
domkniecie(Gramatyka, [Sytuacja | Ogon], Stara, Nowa) :-
	(
		memberchk(Sytuacja, Stara)-> % Czy produkcja już przetworzona
			domkniecie(Gramatyka, Ogon, Stara, Nowa)
		;
			/*	Jeżeli symbol za kropka jest nieterminalem,
				musimy go dodać jako nieprzetworzony do
				zbioru sytuacji.
			*/
			indukowaneProdukcje(Gramatyka, Sytuacja, DoDodania),
			% W Wynik, Sytuacje, otrzymane z DoDodania.
			produkcje_do_sytuacji(DoDodania, Wynik),
			% Diff = sytuacje, bez obecnie przetwarzanej.
			%subtract(Wynik, [Sytuacja], R),
			delete(Wynik, [Sytuacja], R),
			% Nowe = kolejka oczekujących na przetworzenie, z nowo dodanymi syt.
			append(Ogon, R, Nowe),
			% W KolejnaIteracja lista czekających na przetworzenie, bez duplikatów.
			%list_to_set(Nowe, KolejnaIteracja),
			remove_dups(Nowe, KolejnaIteracja),
			% W Dalej już przetworzone sytuacje
			append(Stara, [Sytuacja], Dalej),
			/*
				Przetwarzaj sytuacje, aż nie ma nieprzetworzonych i
				nie można dodać nowych sytuacji do zbioru.
			*/
			domkniecie(Gramatyka, KolejnaIteracja, Dalej, Nowa)
	).

% indukowaneProdukcje(+Gramatyka, +Sytuacja, -Wyniki)
% Jeżeli za kropką (w Sytuacji) nt, w Wynik wszystkie produkcje dla tego nt.
% Jeżeli za kropką terminal, w Wynik pusta lista.
indukowaneProdukcje(Gramatyka, sytuacja(Nr, Kropka), Wynik) :-
	produkcjaNumer(Gramatyka, Nr, prod(Nr, _, RHS)),
	(nth0(Kropka, RHS, nt(P)) ->% Symbol za kropką nieterminalem?
		produkcjeSymbol(Gramatyka, P, Wynik)
	;
		Wynik = []
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Procedury tworzące wstępny graf, który jest potem przerabiany na tabele automatu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% stworz_graf(+Index, +Gramatyka, +Stany, +Kolejka, -NoweStany).
% W NoweStany, nowo zbudowany graf, w postaci(stan(Nr, ZbiorSytuacji, ListaSasiedztwa)).
stworz_graf( _, _, _, [], []).
stworz_graf(Index, Gramatyka,  Stany, [stan(K, Z) | Kolejka], [stan(K, Z, Krawedzie) | Graf]) :-
	% Nowe = para(sytuacja(Nr, Kropka), X), sytuacja do której można przejść przez X
	obsluz_stan(Gramatyka, Index, stan(K, Z), Nowe),
	stworz_stany(Index, Stany, Nowe, Env, NowyIndex),
	stworz_krawedzie(Env, Nowe, Krawedzie),
	append(Stany, X, Env),
	append(Kolejka, X, Dalej),
	stworz_graf(NowyIndex, Gramatyka, Env, Dalej, Graf).

% obsluz_stan(+Gramatyka, +Stan, -NoweStany)
% W Nowe lista stanów, które są osiągalne z danego stanu
obsluz_stan(Gramatyka, _, stan(_, Sytuacje), Nowy):-
	% W Przejscia możliwe symbole za kropką, dla sytuacji z danego stanu
	za_kropka(Gramatyka, Sytuacje, Y),
	% W Krawedzie pogrupowane zbiory sytuacji, ze względu na symbol do nich prowadązcy
	grupuj_symbole(Gramatyka, Y, [], Z),
	% W Nowy, wszystkie możliwe stany, osiągalne z danego stanu
	nowy_zbior(Gramatyka, Z, Nowy).



% za_kropka(+Gramatyka, +Sytuacje, -Przejscia)
% W Przejscia wszystkie sytuacje osiągalne z Sytuacji, wraz z informacją
% po jakim symbolu możemy dostać się do danej sytuacji(Sytuacja, Symbol)
za_kropka(_, [], []).
za_kropka(Gramatyka, [sytuacja(Nr, Kropka) | Ogon], To):-
	za_kropka(Gramatyka, Ogon, Zbiory),
	produkcjaNumer(Gramatyka, Nr, prod(Nr, _, RHS)),
	length(RHS, N),
	(Kropka < N -> % Czy kropka nie na końcu produkcji
		nth0(Kropka, RHS, X),
		To = [para(sytuacja(Nr, Kropka), X) | Zbiory]
	;
		To = Zbiory % Kropka za daleko, nie ma możliwej krawędzi
	).


% grupuj_symbole(+Gramatyka, +OsiagalneStany, ?Akumulator, -Wynik).
% W Wynik sytuacje pogrupowane względem symbolu, który do nich prowadzi
% Para może być jako jeden term, ale dla przejrzystości, co ta para zawiera:
grupuj_symbole(_, [], Acc, Acc).
grupuj_symbole(Gramatyka, [para(Syt, Symbol) | Pary], Stary, Wynik) :-
	dodaj_przejscie(para(Syt, Symbol), Stary, Nowy),
	grupuj_symbole(Gramatyka, Pary, Nowy, Wynik).

% dodaj_przejscie(+MożliwaSytuacja, ?Akumulato, -Nowa)
% W Nowa lista pogrupowanych stanów(względem symbolu do nich prowadzącego),
% uzyskana z dodania MożliwaSytuacja do obecnej listy Stara.
% Pogrupowanie mają postać: para(Sytuacje, Symbol)
dodaj_przejscie(para(sytuacja(Nr, Poz), Symbol), Stara, Nowa) :-
	M is Poz + 1,
	(\+ member(para(X, Symbol), Stara) ->
		% Nie istnieje grupa dla danego symbolu
		append(Stara, [para([sytuacja(Nr, M)], Symbol)], Nowa)
	;
		% W X obecna grupa sytuacji dla Symbol. W Lista usunięto tę parę
		select(para(X, Symbol), Stara, Lista),
		% Dodanie zaktualizowanej grupy
		append(Lista, [para([sytuacja(Nr, M) | X], Symbol)], Nowa)
	).

% nowy_zbior (+Gramatyka, +ZbiorySytuacji, -NoweZbiory)
% W NoweZbiory, domknięte zbiory sytuacji uzyskane z danych grup sytuacji.
nowy_zbior(_, [], []).
nowy_zbior(G, [para(H, S) | T], Final):-
	domkniecie(G, H, [], Wynik),
	nowy_zbior(G, T, Koniec),
	append([para(Wynik, S)], Koniec, Final).


% stworz_krawedzie(+Stany, +PotencjalnePrzejścia, -Krawedzie)
% W Krawedzie, lista sąsiedztwa dla danego zbioru potencjalnych przejść do stanów
% krawedz(Symbol, NrStanu) Symbol na krawędzi, NrStanu do którego stanu prowadzi
stworz_krawedzie(_, [], []).
stworz_krawedzie(Stany, [para(Z, S) | Zbiory], [krawedz(S, Numer) | Krawedzie]):-
	member(stan(Numer, Z), Stany),
	stworz_krawedzie(Stany, Zbiory, Krawedzie).

% zwroc_stany(+Para(Sytuacje, Symbol), -Sytuacje).
% W Sytuacja, wyjęte z listy par sytuacje (odpowiednik fst dla pair w cpp)
zwroc_stany([], _).
zwroc_stany([para(Sytuacje, _) | Reszta], [Sytuacje | Wynikowe]) :-
	zwroc_stany(Reszta, Wynikowe).

% stworz_stany(+Nr, +Stany, +Nowe, -Wynik, -Nowy_index).
% Wynikowe = Zbiór stanów rozszerzony o nowe stany w Nowe.
% Nowy_index = pierwszy wolny indeks dla numeracji stanów.
stworz_stany(N, G, [], G, N).
stworz_stany(Numer, Stany, [para(Sytuacje, _) | Reszta], Wynikowe, Nowy_index) :-
	% Stan o zawartośc Sytuacje już istnieje.
	(member(stan(_, Sytuacje), Stany) ->
		stworz_stany(Numer, Stany, Reszta, Wynikowe, Nowy_index)
	;
		append(Stany, [stan(Numer, Sytuacje)], Dodane),
		M is Numer + 1,
		stworz_stany(M, Dodane, Reszta, Wynikowe, Nowy_index)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tworzenie tabeli dla automatu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% automat(Akcje, Redukcje)
% Trochę, niż w oryginalnym algorytmie, trzymam tabelę.
% W Akcje trzymam jednocześnie przesunięcia i przejścia,
% a w Redukcje redukcje oraz akceptacje.

% stan(Nr, Sytuacje, Krawedzie)
% przesun(StanSkąd, Symbol, StanDokąd)
% przejscie(StanSkąd, Symbol, StanDokąd)

% stworz_tabele(+Gramatyka, +Graf, -Akcje, -Redukcje)
% W Akcje i Redukcje, tablice stworzone z grafowej postaci automatu
stworz_tabele(_, [], [], []).
stworz_tabele(Gramatyka, [Stan | Graf], [AkcjaF | AOgon],[ RedukcjaF | ROgon]) :-
	redukcje(Gramatyka, Stan, Redukcja),
	dodaj_wpis(Stan, Akcje),
	my_flatten(Akcje, AkcjaF),
	my_flatten(Redukcja, RedukcjaF),
	stworz_tabele(Gramatyka, Graf, AOgon, ROgon).

% Redukcja Z -> S#
czy_akceptujacy(0, 1).



% redukcje(+Gramatyka, +Stany, -Redukcje)
% W Redukcje nowo wykryta redukcje lub akceptacja dla danego stanu.
% Możliwe wiele, dopiero potem sprawdzanie takiego konfliktu.
redukcje(_, stan(_,[],_),[]).
redukcje(Gramatyka, stan(Nr, [sytuacja(Id, Poz) | Sytuacje], _), [Ruch| Redukcje]) :-
	(czy_akceptujacy(Id, Poz) ->
		Ruch = akceptuj(Nr, $)
	;
		produkcjaNumer(Gramatyka, Id, prod(_, LHS, RHS)),
		length(RHS, Dlg),
		(Dlg = Poz ->
			Ruch = redukcja(Nr, LHS, Dlg)
		;
			Ruch = []
		)
	),
	redukcje(Gramatyka, stan(Nr, Sytuacje, _), Redukcje).


% dodaj_wpis(+Stan, -Akcje)
% W Akcje tabela z przejściami i przesunięciami.
% kolumny dla symboli nieterminalnych są kopiowane do tabeli goto(tutaj przejscie)
dodaj_wpis(stan(_, _, []),[]).
dodaj_wpis(stan(Nr, Sytuacje, [krawedz(nt(X), Cel) | Krawedzie]), [przejscie(Nr, X, Cel) | Reszta]) :-
	dodaj_wpis(stan(Nr, Sytuacje, Krawedzie), Reszta),!.
% kolumny dla symboli terminalnych są kopiowane do tabeli action jako akcje przesunięcia(shift)
dodaj_wpis(stan(Nr, Sytuacje, [krawedz(X, Cel)| Krawedzie]), [przesun(Nr, X, Cel)| Reszta]) :-
	dodaj_wpis(stan(Nr, Sytuacje, Krawedzie), Reszta).


%%%%%%%%%%%%%%%%%%%%%%%
% Wykrywanie konfliktów

% konfliktSR(Info, Stan, SymbolPrzesuniecia, SymbolRedukcji)
konfliktSR('Konflikt przesuniecie-redukcja', _, _, _).
% konfliktSR(Info, Redukcje)
konfliktRR('Konflikt redukcja-redukcja', _).


% sprawdz_konflikt(+Redukcje, +Akcje, -Info).
% Info = yes jeżeli w tablicy nie ma konfliktu przejście-redukcja.
sprawdz_konflikt([], _, yes).
sprawdz_konflikt([akceptuj(_, _) | Redukcje], Akcje, Info) :-
	sprawdz_konflikt(Redukcje, Akcje, Info).
sprawdz_konflikt([redukcja(Nr, Nt, _) | Redukcje], Akcje, Info) :-
	(  member(przesun(Nr, S,_), Akcje) ->%to musi byc term osobny
		Info =
			konflikt(konfliktSR('Konflikt przesuniecie-redukcja', Nr, S, Nt))
	;
		sprawdz_konflikt(Redukcje, Akcje, Info)
	).

% sprawdz_redukcje(+Redukcje, +Akcje, +ZbioryRedukcji, -Info)
% Sprawdź czy w tablicy nie ma konfliktu redukcja-redukcja
% Redukcje - spłaszczone Redukcje(przekazane potem do sprawdz_konflikt),
% a w ZbioryRedukcji, zbiory redukcji dla danego stanu.
sprawdz_redukcje(Goto, Akcja, [], Info) :-
	sprawdz_konflikt(Goto, Akcja, Info). % Nie ma konfliktuRR, sprawdź SR
sprawdz_redukcje(Goto, Akcja, [H | Reszta], Info) :-
	length(H, Len),
	( Len > 1 -> % Więcej niż jedna redukcja dla stanu
		Info = konflikt(konfliktRR('Konflikt redukcja-redukcja', H))
	;
		sprawdz_redukcje(Goto, Akcja, Reszta, Info)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Właściwy Analizator LR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% automat(Akcje, Redukcje).

% analizator(+Automat, ?Stos, +Slowo).
% Symuluje działania parsera LR.
analizator(_, _,[], no).
analizator(automat(Akcje, Redukcje), [stan(S) | Stos],[A | Slowo], Jak):-%!,
	(
		% akcja[S, A] = przesun(Nowy)
		member(przesun(S, A, Nowy), Akcje)->
			analizator(automat(Akcje, Redukcje), [stan(Nowy), A, stan(S) | Stos], Slowo, Jak)
		;
			(
				member(akceptuj(S, A), Redukcje)->
					sukces % Słowo zaakceptowane
				;
					%Czy akcja[S, A] = redukuj ?
					czy_redukowac(automat(Akcje, Redukcje), S, Stos, [A | Slowo], Jak)
			)
	).

sukces.

% czy_redukowac(+Automat, +Stan, +Stos, +Slowo)
% Sprawdza czy istnieje redukcja dla aktualnego stanu.
czy_redukowac(automat(Akcje, Redukcje), S, Stos, [A | Slowo], Jak):-
	(
		member(redukcja(S, LHS, Len), Redukcje) ->
			M is 2 * Len, % Zdejmij ze stosu 2 * dlugosc produkcji symboli
			split([S|Stos], M, _, [stan(SPrim) | NowyStos]),
			% SPrim nowy wierzchołkiem stosu
			member(przejscie(SPrim, LHS, Gdzie), Akcje),
			analizator(automat(Akcje, Redukcje), [ stan(Gdzie) , LHS ,stan(SPrim)|NowyStos], [A | Slowo], Jak)
		;
			Jak = no, !, fail % Słowo nieakceptowane
	).


% createLR(+Gramatyka, -Automat, -Info)
createLR(gramatyka(Start, G), Automat, Info) :-
	% Gramatyka = zmieniona forma gramatyki wejściowej
	moja_gramatyka(Start, G, Gramatyka),
	% Proces tworzenia automatu zaczyna się od domknięcia sytuacji Z-> .S#
	domkniecie(Gramatyka, [sytuacja(0,0)], [], X),
	stworz_graf(1, Gramatyka, [stan(0,X)], [stan(0,X)], Graf),
	% Na podstawie grafu tworzymy tabele
	stworz_tabele(Gramatyka, Graf, Akcja, Goto),
	my_flatten(Akcja, AkcjaF),
	my_flatten(Goto, GotoF),
	% Sprawdzamy czy nie mamy konflików,
	% sprawdz redukcje, jeżeli nie wykryje konflików wywoła sprawdzanie SR
	sprawdz_redukcje(GotoF, AkcjaF, Goto, Info),
	( Info = yes ->
		Automat = automat(AkcjaF, GotoF)
	;
		Automat = null
	).

% accept(+Automat, +Słowo)
accept(automat(Akcje, Redukcje), Slowo) :-
	append(Slowo, [$], Slow),
	analizator(automat(Akcje, Redukcje), [stan(0)], Slow, _).



% split(L,N,L1,L2).
% Pomocnicza procedura
% W L1 lista zawierająca N pierwszych elementów L,
% w L2 reszta listy.
% Kod zaczerpnięty z 99 problems of prolog
split(L,0,[],L).
split([X|Xs],N,[X|Ys],Zs) :- N > 0, N1 is N - 1, split(Xs,N1,Ys,Zs).

% W swipl mamy procedure flatten
% Pod sictusem używam:
% my_flatten(L1,L2) :- the list L2 is obtained from the list L1 by
%    flattening; i.e. if an element of L1 is a list then it is replaced
%    by its elements, recursively.
%    (list,list) (+,?)
my_flatten(X,[X]) :- \+ is_list(X).
my_flatten([],[]).
my_flatten([X|Xs],Zs) :- my_flatten(X,Y), my_flatten(Xs,Ys), append(Y,Ys,Zs).
