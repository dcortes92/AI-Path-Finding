% Instituto Tecnológico de Costa Rica
% Escuela de Ingeniería en Computación
% Inteligencia Artificial
% I Tarea Programada 
% Daniel Cortés Sáenz, 201120152
% Descripción: implementación de algorítmos de búsqueda de ruta óptima informados:
%				1. Greedy: best first, basado en tomar la ruta con el menor h(x)
%				2. A*: se basa en tomar la ruta con el menor F(x) = G(x) + H(x)
%				3. Jumping positions.

-module(search).
-author('dcortes92@hotmail.com').
-import(board). %para utilizar las mismas funciones que se usan en board
-export([greedy/0, min/1, greedy_algorithm/2, euclides/2, astar/0, astar_algorithm/3, 
		astar_algorithm_proc_vecinos/5, agregarOpen/3, menor_f/1, esta/3]).


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
				{Fringes, Heuristicas} -> %En otro caso, se mueve a la mejor celda y se quita esa
										 %celda de las celdas visitadas.
                    {Minimo, Indice} = min(Heuristicas),
                    Siguiente = lists:nth(Indice, Fringes),
                    board ! {move, Siguiente},
                    NuevoFringe = lists:delete(Siguiente, Fringes),
					NuevoHeuristica = lists:delete(Minimo, Heuristicas),
					%Se agregan a las heuristicas y los vecinos al frige a las estructuras ya creadas
					%excepto el del campo escodgido.
                    greedy_algorithm({X, Y}, {lists:append(Fringe, NuevoFringe), lists:append(Heuristica, NuevoHeuristica)}) 
			end
	end.

%Obtiene el indice del menor de la lista junto con su indice
min([]) -> error;
min([H]) -> {H, 1};
min([H|T]) -> min_aux(T, H, 1, 2).

min_aux([], Min, IndiceMin, _) -> {Min, IndiceMin};
min_aux([T|C], Min, _, IndiceActual) when(T < Min) -> min_aux(C, T, IndiceActual, IndiceActual+1);
min_aux([_|C], Min, IndiceMin, IndiceActual) -> min_aux(C, Min, IndiceMin, IndiceActual+1).

%Algoritmo A*.
astar() ->
	board ! {get_goal, self()},
	receive
		{X, Y} -> 
			board ! {get_pos, self()},
			receive										 %Lista open, contiene el nodo inicial
														 %			 Padre, G, H
				{A, B} -> spawn(search, astar_algorithm, [[[{A, B}, {A, B}, 0, euclides({A,B},{X,Y})]],
														 %Lista closed
														 [],
														 %Objetivo
														 {X,Y}])
			end
	end.

astar_algorithm(Open, Closed, {X,Y}) ->
	board ! {get_pos, self()},
	receive
		{I, J} when (I == X) and (J == Y) -> %Primero se pregunta si se ha llegado al objetivo
			%Construir ruta
			%io:format("~p~n", [Closed]), %Descomentar esta línea para ver cómo se reconstruye la ruta
			Ruta = astar_reconstruir_ruta(Closed),
			io:format("~n~nRuta encontrada:~n~n"),
			io:format("~p~n", [lists:reverse(Ruta)]),
			epicwin;
		{_, _} ->
			if Open == [] ->
				io:format("~n~n*** Ruta no encontrada. ***"),
				epicfail;
			true -> %Comienza el algoritmo
				% 1. Se obtiene el menor 
				{MenorF, Indice} = menor_f(Open),
				% 2. Se mueve a la lista cerrada
				NuevaClosed = lists:append([MenorF], Closed),
				% 4. Se borra de la lista abierta y nos movemos a esa posición
				NuevaOpen = lists:delete(Indice, Open),
				board ! {move, lists:nth(1, MenorF)},
				% 5. Se hace para cada uno de los vecinos
				board ! {get_neighbors, self()},
				receive 
					{[], _} -> %No hay vecinos pero queda algo en la lista Open
						{MenorF, Indice} = menor_f(NuevaOpen), %Se obtiene el menor
						board ! {move, lists:nth(1, MenorF)}, %Y nos movemos ahí, lo borramos de Open
						astar_algorithm(lists:delete(lists:nth(Indice, NuevaOpen), NuevaOpen), Closed, {X, Y}); %Backtrack
						
					{Vecinos, _} -> %Si hay vecinos
						%Procesar cada uno de los vecinos y obtener una nueva lista open
						astar_algorithm(
							astar_algorithm_proc_vecinos(Vecinos, NuevaOpen, NuevaClosed, MenorF, {X,Y}), 
							NuevaClosed, {X, Y})
				end
			end
	end.

%Se reconstruye la ruta recorriendo la lista Closed.
%Imprimir la lista Closed para entender la idea (línea 90).
%A veces, se imprime en esta lista el cuadro objetivo. Esto pasa porque
%el algoritmo debe detenerse cuando se agregue el X,Y objetivo a la lista open
%sin embargo, en este caso, el algoritmo se detiene hasta que se está posicionado
%en el cuadro objetivo.
astar_reconstruir_ruta([[{X, Y}, {PadreX, PadreY}, _, _]|T]) ->
	[{X, Y}] ++ astar_reconstruir_ruta_aux(T, {PadreX, PadreY}).

astar_reconstruir_ruta_aux([[{X, Y}, {PadreX, PadreY}, _, _]|T], {PadreX1, PadreY1}) 
	when (X == PadreX1) and (Y == PadreY1) ->
		[{X, Y}] ++ astar_reconstruir_ruta_aux(T, {PadreX, PadreY});

astar_reconstruir_ruta_aux([_|T], {X, Y}) ->
	astar_reconstruir_ruta_aux(T, {X, Y});

astar_reconstruir_ruta_aux([], _) -> [].


%Procesar los vecinos del cuadro actual
							%vecinos  						%Objetivo
astar_algorithm_proc_vecinos([H|T], Open, Closed, NodoActual, {X,Y}) ->
	EstaClosed = esta(H, Closed, 0),
	CurrentGValue = lists:nth(3, NodoActual),
	if EstaClosed /= 0->
		NodoVecino = lists:nth(EstaClosed, Closed), %Obtiene el Vecino
		GValue = lists:nth(3, NodoVecino),
		HValue = lists:nth(4, NodoVecino),
		if CurrentGValue =< GValue ->
			NuevaClosed = lists:delete(NodoVecino, Closed),
			astar_algorithm_proc_vecinos(T, 
			lists:append([H, lists:nth(2, NodoActual), CurrentGValue, HValue], Open), 
			NuevaClosed, NodoActual, {X,Y});
		true ->
			astar_algorithm_proc_vecinos(T, Open, Closed, NodoActual, {X,Y})
		end;
	true ->
		EstaOpen = esta(H, Open, 0),
		if EstaOpen /= 0 ->
			NodoVecino = lists:nth(EstaOpen, Open), %Obtiene el Vecino
			GValue = lists:nth(3, NodoVecino),
			HValue = lists:nth(4, NodoVecino),
			if CurrentGValue =< GValue ->
				%Si and CurrentGValue < GValue
				%Cambiar g del vecino y el padre del vecino sería el nodo actual
				NuevaOpen = lists:delete(NodoVecino, Open),
				astar_algorithm_proc_vecinos(T, 
					lists:append([H, lists:nth(2, NodoActual), CurrentGValue, HValue], NuevaOpen), 
					Closed, NodoActual, {X,Y});
			true ->
				astar_algorithm_proc_vecinos(T, Open, Closed, NodoActual, {X,Y})
			end;
		true ->
			NuevaOpen = agregarOpen(H, {X,Y}, NodoActual),
			astar_algorithm_proc_vecinos(T, lists:append(NuevaOpen, Open), Closed, NodoActual, {X,Y})
		end
	end;


astar_algorithm_proc_vecinos([], Open, _, _, _) ->
	Open.


%Para saber si un punto x,y está en una lista. Se usa en el A* para saber
%si un nodo está o no en alguna de las listas closed u opened.
esta(_, [], _) -> 0;
esta({X,Y}, [H|T], C) ->
	{I,J} = lists:nth(1, H),
	if (I == X) and (J == Y)->
		C;
	true->
		esta({X,Y}, T, C+1)
	end.

%Distancia euclediana entre 2 puntos
euclides({X, Y}, {W, Z}) -> math:sqrt(math:pow(X-W, 2) + math:pow(Y-Z, 2)).


%Agrega cada elemento al open de la forma [[{X,Y}, G({X,Y}), H({X,Y})]]
agregarOpen([H|T], {X,Y}, NodoActual) ->
[
	[H, B, C, D] ||
		B <- [lists:nth(1, NodoActual)], 
		C <- [lists:nth(3, NodoActual) + 1], % El costo de moverse en cualquier dirección es 1
		D <- [euclides(H, {X,Y})]
] ++ agregarOpen(T, {X,Y}, NodoActual);

agregarOpen(H, {X,Y}, NodoActual) ->
[
	[H, B, C, D] || 
		B <- [lists:nth(1, NodoActual)], 
		C <- [lists:nth(3, NodoActual) + 1], % El costo de moverse en cualquier dirección es 1
		D <- [euclides(H, {X,Y})]
];

agregarOpen([], _, _) -> [].

%Obtiene el cuadro con el menor F de la lista y su posición en esta lista (para luego borrarlo)
menor_f([]) -> [];
menor_f([H]) -> {H,1}; %Si solo hay un elemento retorne ese punto X,Y
menor_f([H|T]) -> menor_f_aux(T, H, 1, 2).

menor_f_aux([], Menor, IndiceMin, _) -> {Menor, IndiceMin};
menor_f_aux([T|C], Menor, IndiceMin, IndiceActual) -> 
	FT = calcular_f(lists:nth(3, T), lists:nth(4, T)),
	FMen = calcular_f(lists:nth(3, Menor), lists:nth(4, Menor)),
	if(FT < FMen) ->
		menor_f_aux(C, T, IndiceActual, IndiceActual+1);
	true ->
		menor_f_aux(C, Menor, IndiceMin, IndiceActual+1)
	end.

calcular_f(G, H) -> G + H.