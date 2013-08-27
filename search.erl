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
-export([greedy/0, min/1, greedy_algorithm/2, esta/2, astar/0, astar_algorithm/6, prueba/1]).

%Obtiene el indice del menor de la lista junto con su indice
min([]) -> error;
min([H]) -> {H, 1};
min([H|T]) -> min_aux(T, H, 1, 2).

min_aux([], Min, IndiceMin, _) -> {Min, IndiceMin};
min_aux([T|C], Min, _, IndiceActual) when(T < Min) -> min_aux(C, T, IndiceActual, IndiceActual+1);
min_aux([_|C], Min, IndiceMin, IndiceActual) -> min_aux(C, Min, IndiceMin, IndiceActual+1).

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
			receive 											  %current,parent,f 
				{A, B} -> spawn(search, astar_algorithm, [{X, Y}, [{A,B}, {}, {}], {}, [{A,B}], [], []]) %Se agrega el nodo inicial a la 																			%lista open
			end
	end.

			  %objetivo
astar_algorithm({X, Y}, Current, Open, Closed, Vecinos, Ruta) ->
	Esta = esta({X,Y}, Open),
	if  Esta == yes -> found; %si se agrega el objetivo a la lista closed 
	true -> 
		if Open == [] -> error; %No hay ruta
		true ->
			board ! {get_neighbors, self()},
			receive
				{Vecinos, _} -> Vecinos
			end
		end
	end.



%Para saber si un punto x,y está en una lista. Se usa en el A* para saber
%si un nodo está o no en alguna de las listas closed u opened.
esta({_,_}, []) -> [];
esta({X, Y}, [{X, Y}|_]) -> yes;
esta({X, Y}, [H|T]) -> esta({X,Y}, T).


prueba(Vecinos, Open, Closed) ->
	%Procesar cada uno de los vecinos
	Procesar = fun(Elemento) -> 
		Esta = esta(Elemento, Closed),
		if Esta == yes -> %Si está en closed se ignora
			lists:delete(Vecinos, Elemento)
		true-> %En otro caso preguntamos para ver si está en la open
			Esta = esta(Elemento, Open),
			if Esta == yes -> 
			true -> %Si no está agregarlo a open y hacer current padre de este
				lists:append(Open, Elemento),

		end
	end,
	lists:foreach(Procesar, [{1,1},{2,3}]).