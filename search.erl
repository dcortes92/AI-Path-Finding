% Instituto Tecnológico de Costa Rica
% Escuela de Ingeniería en Computación
% Inteligencia Artificial
% I Tarea Programada 
% Daniel Cortés Sáenz, 201120152
% Descripción: implementación de algorítmos de búsqueda de ruta óptima informados:
%				1. Greedy:
%				2. A*:
%				3. Jumping positions.

-module(search).
-author('dcortes92@hotmail.com').
-import(board). %para utilizar las mismas funciones que se usan en board
-export([greedy/0, minimum/1, min/1, greedy_algorithm/2]).

%Obtiene el indice del menor de la lista junto con su indice
min([]) -> error;
min([H]) -> {H, 1};
min([H|T]) -> min_aux(T, H, 1, 2).

min_aux([], Min, IndiceMin, _) -> {Min, IndiceMin};
min_aux([T|C], Min, _, IndiceActual) when(T < Min) -> min_aux(C, T, IndiceActual, IndiceActual+1);
min_aux([_|C], Min, IndiceMin, IndiceActual) -> min_aux(C, Min, IndiceMin, IndiceActual+1).

minimum([]) -> error;
minimum([Min]) -> {Min, 1};
minimum([Head|Tail]) -> minimum(Head, Tail, 1, 2).
minimum(Min, [Head|Tail], Index, CIndex) ->
    if
        Head < Min ->
            minimum(Head, Tail, CIndex, CIndex + 1);
        true ->
            minimum(Min, Tail, Index, CIndex + 1)
    end;
minimum(Min, [], Index, _) -> {Min, Index}.

% Lógica del algoritmo greedy
greedy_algorithm({X, Y}, {Fringe, Heuristica}) -> 
	board ! {get_pos, self()},
	receive
		{I, J} when (I == X) and (J == Y) -> found; %Primero se pregunta si se ha llegado al objetivo
		{_, _} -> 
			board ! {get_neighbors, self()}, %En otro caso se obtienen los vecinos y se continua con el algoritmo
			receive 
				{[], _} -> 
					if Fringe == [] -> io:format("No se ha encontrado una ruta.~n"), fail; %En caso de que no se encuentre una ruta
					true -> %Si ya no hay vecinos pero queda algo en el fringe
						{Minimo, Indice} = minimum(Heuristica),
						Siguiente = lists:nth(Indice, Fringe),
						board ! {move, Siguiente},
						greedy_algorithm({X,Y}, {lists:delete(Siguiente, Fringe), lists:delete(Minimo, Heuristica)})
					end;
				{Celdas, Heuristicas} -> %En otro caso, se agrega el menor a la lista de fringe
					{Min, Indice} = minimum(Heuristicas),
					Siguiente = lists:nth(Indice, Celdas),
					board ! {move, Siguiente},
					greedy_algorithm({X, Y}, {lists:append(Fringe, lists:delete(Siguiente, Celdas)),
											  lists:append(Heuristica, lists:delete(Min, Heuristicas))})
			end
	end.


greedy() -> 
	board ! { get_goal, self() }, 
	receive 
		{X, Y} -> spawn(search, greedy_algorithm, [{X, Y}, {[], []}])
	end.