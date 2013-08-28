% Instituto Tecnológico de Costa Rica
% Escuela de Ingeniería en Computación
% Inteligencia Artificial
% I Tarea Programada 
% Daniel Cortés Sáenz, 201120152
% Descripción: implementación de algorítmos de búsqueda de ruta óptima informados:
%				1. Greedy: best first, basado en tomar la ruta con el menor h(x)
%				2. A*:
%				3. Jumping positions.

-module(search).
-author('dcortes92@hotmail.com').
-import(board). %para utilizar las mismas funciones que se usan en board
-export([greedy/0, min/1, greedy_algorithm/2, g/2, euclides/2, astar/0, astar_algorithm/4, 
		agregarOpen/4, menor_f/1, esta/2]).

%Obtiene el indice del menor de la lista junto con su indice
min([]) -> error;
min([H]) -> {H, 1};
min([H|T]) -> min_aux(T, H, 1, 2).

min_aux([], Min, IndiceMin, _) -> {Min, IndiceMin};
min_aux([T|C], Min, _, IndiceActual) when(T < Min) -> min_aux(C, T, IndiceActual, IndiceActual+1);
min_aux([_|C], Min, IndiceMin, IndiceActual) -> min_aux(C, Min, IndiceMin, IndiceActual+1).

%Obtiene el cuadro con el menor F de la lista y su posición en esta lista (para luego borrarlo)
menor_f([]) -> [];
menor_f([H]) -> {H,1}; %Si solo hay un elemento retorne ese punto X,Y
menor_f([H|T]) -> menor_f_aux(T, H, 1, 2).

menor_f_aux([], Menor, IndiceMin, _) -> {Menor, IndiceMin};
menor_f_aux([T|C], Menor, IndiceMin, IndiceActual) -> 
	FT = calcular_f(lists:nth(2, T), lists:nth(3, T)),
	FMen = calcular_f(lists:nth(2, Menor), lists:nth(3, Menor)),
	if(FT < FMen) ->
		menor_f_aux(C, T, IndiceActual, IndiceActual+1);
	true ->
		menor_f_aux(C, Menor, IndiceMin, IndiceActual+1)
	end.

calcular_f(G, H) -> G + H.

%Algoritmo greedy best-first.
greedy() -> 
	board ! { get_goal, self() }, 
	receive 
		{X, Y} -> spawn(search, greedy_algorithm, [{X, Y}, {[], []}])
	end.

% Lógica del algoritmo greedy
greedy_algorithm({X, Y}, {Fringe, Heuristica}) ->  
	board ! {get_pos, self()},
	receive
		{I, J} when (I == X) and (J == Y) -> %Primero se pregunta si se ha llegado al objetivo
			io:format("Ruta encontrada."),
			epicwin;
		{_, _} -> 
			board ! {get_neighbors, self()}, %En otro caso se obtienen los vecinos y se continua con el algoritmo
			receive 
				{[], _} -> %Si ya no hay vecinos
					if Fringe == [] -> %Si no hay vecinos y el fringe está vacío
						io:format("No se ha encontrado una ruta."), 
						fail; %En caso de que no se encuentre una ruta
					true -> %Else si ya no hay vecinos pero queda algo en el fringe
						{Minimo, Indice} = min(Heuristica),
						Siguiente = lists:nth(Indice, Fringe),
						board ! {move, Siguiente}, 
						NuevoFringe = lists:delete(Siguiente, Fringe), %Se quita el siguiente del fringe con su heuristica
						NuevoHeuristica = lists:delete(Minimo, Heuristica),
						greedy_algorithm({X, Y}, {NuevoFringe,NuevoHeuristica}) %Para devolverse
					end;
				{Celdas, Heuristicas} -> %En otro caso, se mueve a la mejor celda y se quita esa
										 %celda de las celdas visitadas.
                    {Minimo, Indice} = min(Heuristicas),
                    Siguiente = lists:nth(Indice, Celdas),
                    board ! {move, Siguiente},
                    NuevoFringe = lists:delete(Siguiente, Fringe),
					NuevoHeuristica = lists:delete(Minimo, Heuristica),
					%Se agregan a las heuristicas y los vecinos al frige a las estructuras ya creadas
					%excepto el del campo escodgido.
                    greedy_algorithm({X, Y}, {lists:append(Fringe, NuevoFringe), lists:append(Heuristica, NuevoHeuristica)}) 
			end
	end.

%Algoritmo A*.
astar() ->
	board ! {get_goal, self()},
	receive
		{X, Y} -> 
			board ! {get_pos, self()},
			receive										 %Lista open
				{A, B} -> spawn(search, astar_algorithm, [[[{A, B}, 0, euclides({A,B}, {X,Y})]],
														 %Actual
														 [{A, B}, 0, euclides({A,B}, {X,Y})],
														 %Objetivo
														 {X,Y},
														 %Lista de padres
														 []])
			end
	end.

%Logica del algoritmo astar
astar_algorithm(Open, Actual, {X, Y}, Padres) -> 
	board ! {get_pos, self()},
	receive
		{I, J} when (I == X) and (J == Y) -> %En caso de que estemos en el objetivo
			io:format("Encontrado, calculando ruta...~n"),
			epicwin;
		{_,_} ->
			%Buscar el cuadro con el menor F en la lista abierta
			board ! {get_neighbors, self()},
			receive 
				{[], _} -> %Si ya no hay vecinos
					if Open == [] -> %Si no hay vecinos y el fringe está vacío
						io:format("No se ha encontrado una ruta."), 
						fail; %En caso de que no se encuentre una ruta
					true -> %Else si ya no hay vecinos pero queda algo en el fringe
						{Minimo, Indice} = menor_f(Open),
						Siguiente = lists:nth(Indice, Open),
						board ! {move, lists:nth(1, Siguiente)}, 
						NuevoOpen = lists:delete(Siguiente, Open), %Se quita el siguiente del fringe con su heuristica
						astar_algorithm(NuevoOpen, Siguiente, {X, Y}, Padres) %Para devolverse
					end;
				{Celdas, _} -> %En otro caso, se mueve a la mejor celda y se quita esa
										 %celda de las celdas visitadas.
					OpenTemp = agregarOpen( Celdas, {X,Y}, lists:nth(2, Actual), lists:nth(1, Actual) ),
                    {Minimo, Indice} = menor_f(OpenTemp),
                    Siguiente = lists:nth(Indice, OpenTemp),
                    board ! {move, lists:nth(1,Siguiente)},
                    NuevoOpen = lists:delete(Siguiente, OpenTemp),
					%Se agregan a las heuristicas y los vecinos al frige a las estructuras ya creadas
					%excepto el del campo escodgido.
                    astar_algorithm(lists:append(Open, NuevoOpen), Siguiente, {X, Y}, Padres) 
			end
	end.



%Para saber si un punto x,y está en una lista. Se usa en el A* para saber
%si un nodo está o no en alguna de las listas closed u opened.
esta(_, []) -> 0;
esta({X,Y}, [H|T]) ->
	{I,J} = lists:nth(1, H),
	if (I == X) and (J == Y)->
		1;
	true->
		esta({X,Y}, T)
	end.
 
%Costo de ir de un punto X,Y a otro punto W,Z
g({X, Y}, {W, Z}) when (X /= W) and (Y /= Z) -> 2;
g(_,_) -> 1.


%Distancia euclediana entre 2 puntos
euclides({X, Y}, {W, Z}) -> math:sqrt(math:pow(Z-Y, 2) + math:pow(W-X, 2)).


%Agrega cada elemento al open de la forma [[{X,Y}, G({X,Y}), H({X,Y})]]
agregarOpen([H|T], {X,Y}, Actual, {A1,A2}) ->
[
	[H, B, C] || 
		B <- [Actual + g(H, {A1, A2})],
		C <- [euclides(H, {X,Y})]
] ++ agregarOpen(T, {X,Y}, Actual, {A1,A2});

agregarOpen([], _, _, _) -> [].