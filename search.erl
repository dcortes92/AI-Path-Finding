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
-export([greedy/0, min/1, greedy_algorithm/2, test/1]).

%Obtiene el indice del menor de la lista junto con su indice
min([]) -> error;
min([H]) -> {H, 1};
min([H|T]) -> min_aux(T, H, 1, 2).

min_aux([], Min, IndiceMin, _) -> {Min, IndiceMin};
min_aux([T|C], Min, _, IndiceActual) when(T < Min) -> min_aux(C, T, IndiceActual, IndiceActual+1);
min_aux([_|C], Min, IndiceMin, IndiceActual) -> min_aux(C, Min, IndiceMin, IndiceActual+1).

test([_|T]) -> T.

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
			epicwin; 
		{_, _} -> 
			board ! {get_neighbors, self()}, %En otro caso se obtienen los vecinos y se continua con el algoritmo
			receive 
				{[], _} -> 
					if Fringe == [] -> 
						io:format("No se ha encontrado una ruta."), 
						fail; %En caso de que no se encuentre una ruta
					true -> %Else si ya no hay vecinos pero queda algo en el fringe
						{Minimo, Indice} = min(Heuristica),
						Siguiente = lists:nth(Indice, Fringe),
						board ! {move, Siguiente},
						greedy_algorithm({X, Y},
                                    {lists:delete(Siguiente, Fringe),
                                     lists:delete(Minimo, Heuristica)})
					end;
				{Celdas, Heuristicas} -> %En otro caso, se mueve a la mejor celda y se quita esa
										 %celda de las celdas visitadas.
                    {Minimo, Indice} = min(Heuristicas),
                    Siguiente = lists:nth(Indice, Celdas),
                    board ! {move, Siguiente},
                    greedy_algorithm({X, Y}, 
                    	{lists:append(Fringe, lists:delete(Siguiente, Celdas)), %Se agregan al Fringe los demás nodos
                    															%excepto el escogido
                    	lists:append(Heuristica, lists:delete(Minimo, Heuristicas))}) %Se agregan a las heuristicas
                    															%las demás heurísticas excepto la del
                    															%nodo escodgido.
			end
	end.