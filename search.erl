% Instituto Tecnológico de Costa Rica
% Escuela de Ingeniería en Computación
% Inteligencia Artificial
% I Tarea Programada 
% Daniel Cortés Sáenz, 201120152
% Descripción: implementación de algorítmos de búsqueda de ruta óptima informados:
%				1. Greedy: best first, basado en tomar la ruta con el menor h(x)
%				2. A*: se basa en tomar la ruta con el menor F(x) = G(x) + H(x)
%				3. Jumping positions: saltos de posiciones hasta llegar al objetivo. Mejor rendimiento que A*.
%
% Uso: search:greedy()
%	   search:astar()
%	   search:jp()
%
% Incluye comentarios de los bloques de código para cada algoritmo optimizados para el editor sublime-text 2

-module(search).
-author('dcortes92@hotmail.com').
-import(board). %para utilizar las mismas funciones que se usan en board
-export([greedy/0, min/1, greedy_algorithm/2, euclides/2, astar/0, astar_algorithm/3, 
		astar_algorithm_proc_vecinos/5, agregarOpen/3, menor_f/1, esta/3]).
-compile(export_all).
-define(WALL, 1).


%  .oooooo.                                       .o8              
% d8P'  `Y8b                                     "888              
%888           oooo d8b  .ooooo.   .ooooo.   .oooo888  oooo    ooo 
%888           `888""8P d88' `88b d88' `88b d88' `888   `88.  .8'  
%888     ooooo  888     888ooo888 888ooo888 888   888    `88..8'   
%`88.    .88'   888     888    .o 888    .o 888   888     `888'    
% `Y8bood8P'   d888b    `Y8bod8P' `Y8bod8P' `Y8bod88P"     .8'     
%                                                      .o..P'      
%                                                      `Y8P'       

%Algoritmo greedy best-first.
greedy() -> 
	board ! { get_goal, self() }, 
	receive 
								%Le mandamos el Nodo final, {Fringe, Vecinos}
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



%     .o.             .oooooo..o     .                      
%     .888.           d8P'    `Y8   .o8                      
%    .8"888.          Y88bo.      .o888oo  .oooo.   oooo d8b 
%   .8' `888.          `"Y8888o.    888   `P  )88b  `888""8P 
%  .88ooo8888.             `"Y88b   888    .oP"888   888     
% .8'     `888.       oo     .d8P   888 . d8(  888   888     
%o88o     o8888o      8""88888P'    "888" `Y888""8o d888b    


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

%Para saber si un punto está en una lista, se usa en Jump Points porque
%no se necesita saber la posición.
esta(_, []) -> false;
esta(Coordenada, [H | T])->
	if Coordenada == H ->
			true;
	true->
			esta(Coordenada, T)
	end.

%Distancia de euclides entre 2 puntos. Esta es la heurística que usa A*
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


% oooo                                               ooooooooo.              o8o                  .            
%   `888                                               `888   `Y88.            `"'                .o8            
%    888 oooo  oooo  ooo. .oo.  .oo.   oo.ooooo.        888   .d88'  .ooooo.  oooo  ooo. .oo.   .o888oo  .oooo.o 
%    888 `888  `888  `888P"Y88bP"Y88b   888' `88b       888ooo88P'  d88' `88b `888  `888P"Y88b    888   d88(  "8 
%    888  888   888   888   888   888   888   888       888         888   888  888   888   888    888   `"Y88b.  
%    888  888   888   888   888   888   888   888       888         888   888  888   888   888    888 . o.  )88b 
%.o. 88P  `V88V"V8P' o888o o888o o888o  888bod8P'      o888o        `Y8bod8P' o888o o888o o888o   "888" 8""888P' 
%`Y888P                                 888                                                                      
%                                      o888o                                                                     


%Algoritmo Jump Points
jp()-> 
	board ! {get_start, self()},
	receive
		Inicial -> 
		board ! {get_goal, self()},
		receive
			PosFinal ->  
			Open = [[Inicial, 0, 0]], %Declaración de la lista abierta.
			Global = [[Inicial, 0, 0, 1, 0, Inicial]], %Declaración de la lista global.
			Ruta = procesarOpen(Open, Global, PosFinal, [Inicial]), %Para construir la ruta final

			if Ruta /= [] -> %En caso de que la ruta haya sido construida
				Largo = length(lists:nth(2, Ruta)),
				if Largo > 1 -> %Si la lista tiene más de un nodo se encontró la ruta
					RutaJP = jp_reconstruir_ruta(lists:nth(2, Ruta), [], lists:nth(2, Ruta)),
					%Se pinta la ruta en el tablero
					mostrarRuta(RutaJP);
				true -> %EN caso de que no se haya encontrado la ruta
					io:format("~n~n*** Ruta no encontrada. ***")
				end
			end
		end
	end.

%Similar al construir ruta de A*
jp_reconstruir_ruta([T | []], Retorno, _) -> 
	NRetorno = lists:append(Retorno, [T]),
	NRetorno;
jp_reconstruir_ruta([H | T], Retorno, LN) ->
	Padre = lists:nth(1, H),
	Esta = estaPadre(Padre, LN),
	if Esta ->
			NRetorno = lists:append(Retorno, [H]),
			jp_reconstruir_ruta(T, NRetorno, LN);
	true ->
			jp_reconstruir_ruta(T, Retorno, LN)
	end.
						
%Imprime la ruta en el board y en consola pra ver los padres de cada nodo
mostrarRuta([]) -> ok ;
mostrarRuta([Nodo | Resto]) ->
	Coordenada = lists:nth(1, Nodo),
	board ! {jump, Coordenada},
	mostrarRuta(Resto).

%Procesar la lista open
procesarOpen([] , Global, _, _) -> [[], Global];
procesarOpen(Open, Global, Final, NodosVisitados) ->
	Menor = obtenerMinimo(Open),
	NuevaOpen = lists:delete(Menor, Open),
	Coordenadas = lists:nth(1, Menor),
	NuevaGlobal = closeNodo(Coordenadas, Global, []),
	if Coordenadas == Final -> %Se ha llegado al objetivo
			[NuevaOpen, NuevaGlobal];
	true -> 
			%Se mueve en la lista y obtiene nuevos sucesores
			PosSucesores = posiblesSucesores(Menor, NuevaOpen, 
							NuevaGlobal, Final, NodosVisitados),
			procesarOpen(lists:nth(1, PosSucesores), 
				lists:nth(2, PosSucesores), Final, 
				lists:nth(3, PosSucesores))
	end.


%Para identificar los posibles sucesores de un vecino
posiblesSucesores(Menor, Open, Global, {Fx, Fy}, NodosVisitados) ->
	board ! {get_neighborsjp, self()},
	receive
		Vecinos ->
			%Se obtienen sólo los vecinos que no hayan sido visitados
			Neighbors = nuevosVecinos(Vecinos, NodosVisitados),
			if Neighbors == [] ->
					[Open, Global, NodosVisitados];
			true ->
				%Retorna la nuevas listas con las que se debe seguir trabajando
				foreachVecino(Neighbors, Global, Menor, Open, {Fx, Fy}, NodosVisitados)	
			end
	end.

%Retorna la lista de vecinos que no hayan sido visitados
nuevosVecinos([], _) -> [];
nuevosVecinos([H | T], Visitados) ->
	EstaVisitado = esta(H, Visitados),			
	if EstaVisitado ->
			[] ++ nuevosVecinos(T, Visitados);
	true->
			[H] ++ nuevosVecinos(T, Visitados)
	end.

%Recorre la lista de vecinos para irse expandiendo.	
foreachVecino([H | T], ListaGlobal, Nodo = [{X, Y}, G , _], OpenList, {Fx, Fy}, Visitados) -> 
	Vecino = H,
	board ! {movejp, H},
	VisitadosN = lists:append(Visitados, [H]),						
	JumpPoint = jump(Vecino, {X, Y}, {Fx, Fy}),
	if JumpPoint /= [] ->
		{JPx, JPy} = lists:nth(1, JumpPoint),
		Jump = obtenerJumpNodo(ListaGlobal, {JPx, JPy}),
		if Jump == [] ->
			JumpNodo = [{JPx, JPy}, 0, 0, 0, 0, {X, Y}];
		true ->
			JumpNodo = Jump
		end,
		Closed = isclosed({JPx, JPy}, ListaGlobal),
		if Closed ->
				foreachVecino(T, ListaGlobal, Nodo, OpenList, {Fx, Fy}, VisitadosN);
		true -> 
				D = euclides({X, Y},{JPx, JPy}),
				NG = G + D,
				Open = isopened({JPx, JPy}, ListaGlobal),
				JPG = lists:nth(2, JumpNodo),
				if (not Open) or (NG < JPG) ->
						if (not Open) ->
							Heuristica = euclides( {JPx, JPy}, {Fx, Fy} ),
							JumpNodoN_Open_OL = [{JPx, JPy}, NG, Heuristica],
							JumpNodoN_Open_GL = [{JPx, JPy}, NG, Heuristica, 1, lists:nth(5, JumpNodo), {X, Y}],
							OpenListN = lists:append(OpenList, [JumpNodoN_Open_OL]),
							ListaGlobalN = lists:append(ListaGlobal, [JumpNodoN_Open_GL]),
							board ! {movejp, {JPx, JPy}},
							VisitadosNN = lists:append(VisitadosN, [{JPx, JPy}]),
							[OpenListN, ListaGlobalN, VisitadosNN];
							%foreachVecino(T, ListaGlobalN, Nodo, OpenListN, {Fx, Fy}, VisitadosNN);
						true ->
							foreachVecino(T, ListaGlobal, Nodo, OpenList, {Fx, Fy}, VisitadosN)
						end;
				true->
						ListaGlobalN = lists:append(ListaGlobal, [JPG]),
						foreachVecino(T, ListaGlobalN, Nodo, OpenList, {Fx, Fy}, VisitadosN)
				end
		end;
	true ->
			foreachVecino(T, ListaGlobal, Nodo, OpenList, {Fx, Fy}, VisitadosN)
	end;
foreachVecino([], ListaGlobal, _, OpenList, _, Visitados) -> [OpenList, ListaGlobal, Visitados].


estaPadre(_, []) -> false;
estaPadre(Coord, [H | T]) ->
	Temp = lists:nth(6, H),
	if Temp == Coord ->
		true;
	true->
		estaPadre(Coord, T)
	end.


jump(Vecino = {X, Y}, {PX, PY}, NodoFinal)->
	DX = X - PX,
	DY = Y - PY,
	W0 = is_walkable(X, Y),		
	if (not W0) ->
				[];
	Vecino == NodoFinal ->
				[{X, Y}];
	%Checkeo de vecinos forzados en las diagonales
	(DX /= 0) and (DY /= 0) ->
				W1 = is_walkable((X - DX), (Y - DY)),
				W2 = is_walkable((X - DX), Y),
				W3 = is_walkable((X + DX), (Y - DY)),
				W4 = is_walkable(X , (Y - DY)),
				if (W1 and (not W2)) or (W3 and (not W4)) ->
								[{X, Y}];
				true ->
								jump_aux(Vecino, {PX, PY}, NodoFinal)
				end;
	%Sino, diagonales y verticales
	not ((DX /= 0) and (DY /= 0)) ->
				%Moverse sobre el eje X
				if (DX /= 0) ->
						W5 = is_walkable((X + DX), (Y + 1)),
						W6 = is_walkable(X, (Y + 1)),
						W7 = is_walkable((X + DX), (Y - 1)),
						W8 = is_walkable(X, (Y - 1)),
						if (W5 and (not W6)) or (W7 and (not W8)) ->
										[{X, Y}];
						true ->
										jump_aux(Vecino, {PX, PY}, NodoFinal)
						end;
				true ->
						W9  = is_walkable((X + 1), (Y + DY)),
						W10 = is_walkable((X + 1), Y),
						W11 = is_walkable((X - 1), (Y + DY)),
						W12 = is_walkable((X - 1), Y),
						if (W9 and (not W10)) or (W11 and (not W12)) ->
										[{X, Y}];
						true ->
										jump_aux(Vecino, {PX, PY}, NodoFinal)
						end
				end
	end.

jump_aux({X, Y}, {PX, PY}, {Ex, Ey}) ->
	DX = X - PX,
	DY = Y - PY,
	if (DX /= 0) and (DY /= 0) ->
				JX = jump({(X + DX), Y}, {X, Y}, {Ex, Ey}),
				JY = jump({X, (Y + DY)}, {X, Y}, {Ex, Ey}),
				if (JX /= []) or (JY /= []) ->
						[{X, Y}];
				true ->
						jump_aux_2({X, Y}, {PX, PY}, {Ex, Ey})
				end;
	true ->
				jump_aux_2({X, Y}, {PX, PY}, {Ex, Ey})	
	end.

jump_aux_2({X, Y}, {PX, PY}, {Ex, Ey}) ->
	DX = X - PX,
	DY = Y - PY,				
	W13 = is_walkable((X + DX), Y),
	W14 = is_walkable(X, (Y + DY)),
	if (W13 or W14) ->
				jump({(X + DX), (Y + DY)}, {X, Y}, {Ex, Ey});
	true ->
				[]
	end.


filtrarVecinos([], _, Retorno) -> Retorno;
filtrarVecinos([H | T], Visitados, Retorno) ->
	Esta = esta(H, Visitados),			
	if Esta ->
			filtrarVecinos(T, Visitados, Retorno);
	true->
			NRetorno = lists:append(Retorno, [H]),
			filtrarVecinos(T, Visitados, NRetorno)
	end.					


%Averigua si una posicion en el board NO es una pared
is_walkable(X, Y) ->
	board ! {get_all_board, self()},
            receive
                Board -> Board end,
    Pos = board:calcPos(X, Y),
    if
    Pos == invalid -> false;
    true ->
        case element(Pos, Board) of
        ?WALL ->
            false;
        _ ->
            true
        end
    end.

%Obtiene un Nodo de la ListaGlobal, pasandole las coordenadas
obtenerJumpNodo([], _) -> [];
obtenerJumpNodo([H | T], Nodo) -> 
	Coord = lists:nth(1, H),
	if Coord == Nodo -> 
			H;
	true ->
		obtenerJumpNodo(T, Nodo)
	end.

%Busca el estado opened de un Nodo en la ListaGlobal
isopened(_, []) -> false;
isopened(Nodo, [H|T]) ->
	NodoTemp = lists:nth(1, H),
	if Nodo == NodoTemp -> 
		X = lists:nth(4, H),
		if X == 1 -> 
			true;
		true -> 
			false
		end;
	true ->
		isopened(Nodo, T) 
	end.

%Busca el estado closed de un Nodo en la ListaGlobal
isclosed(_, []) -> false;
isclosed(Nodo, [H|T]) ->
	NodoTemp = lists:nth(1, H),
	if Nodo == NodoTemp -> 
		X = lists:nth(5, H),
		if X == 1 -> 
			true;
		true -> 
			false
		end;
	true ->
		isclosed(Nodo, T) 
	end.

%Marca el estado opened = 1 de un Nodo en la ListaGlobal
openNodo(_, [], Retorno) -> Retorno;
openNodo(Nodo, [H | T], Retorno) -> 
	NodoTemp = lists:nth(1, H),
	NodoG = lists:nth(2, H),
	NodoH = lists:nth(3, H),
	NodoClose = lists:nth(5, H),
	Padre = lists:nth(6 , H),
	if Nodo == NodoTemp -> 
		lists:append(Retorno, [[Nodo, NodoG, NodoH, 1, NodoClose, Padre]]);
	true ->
		NRetorno = lists:append(Retorno, [H]),
		openNodo(Nodo, T, NRetorno) 
	end.	

%Marca el estado closed = 1 de un Nodo en la ListaGlobal
closeNodo(_, [], Retorno) -> Retorno;
closeNodo(Nodo, [H | T], Retorno) -> 
	NodoTemp = lists:nth(1, H),
	NodoG = lists:nth(2, H),
	NodoH = lists:nth(3, H),
	NodoOpen = lists:nth(4, H),
	Padre = lists:nth(6 , H),
	if Nodo == NodoTemp -> 
		lists:append(Retorno, [[Nodo, NodoG, NodoH, NodoOpen, 1, Padre]]);
	true ->
		NRetorno = lists:append(Retorno, [H]),
		closeNodo(Nodo, T, NRetorno) 
	end.
	
%Llama al board a moverse a una posicion
moverse(Nodo) ->
	board ! {my_move, Nodo}.
	
%Obtiene la posicion actual del board para guardarla
guardar() ->
	board ! {get_pos, self()},
	receive X->X end.


%Obtiene la cordenada del nodo con el menor F de la lista Open
obtenerMinimo([]) -> [];
obtenerMinimo(OpenList = [H | T]) -> 
	Fs = obtenerFs([H | T], []),
	Min = lists:min(Fs),
	I = indiceMin(Fs, Min, 1),
	lists:nth(I, OpenList).


%Calcula el Fs de los nodos en una lista
obtenerFs([], Final) -> Final;
obtenerFs([H | T], Final) ->
	F = (lists:nth(2, H) + lists:nth(3, H)),
	NFinal = lists:append(Final, [F]),
	obtenerFs(T, NFinal).	


%Encuentra el indice de un numero X en una lista
indiceMin([H|_], Min, I) when H == Min -> I;
indiceMin([_|T], Min, I) -> 
	IS = I + 1,
	indiceMin(T, Min, IS).