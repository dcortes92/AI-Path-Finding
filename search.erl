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
		astar_algorithm_proc_vecinos/5, agregarOpen/3, menor_f/1, esta/3, jp/0]).
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
	RetornoNuevo = lists:append(Retorno, [T]),
	RetornoNuevo;
jp_reconstruir_ruta([H | T], Retorno, LN) ->
	Padre = lists:nth(1, H),
	Esta = estaPadre(Padre, LN),
	if Esta ->
		RetornoNuevo = lists:append(Retorno, [H]),
		jp_reconstruir_ruta(T, RetornoNuevo, LN);
	true ->
		jp_reconstruir_ruta(T, Retorno, LN)
	end.


estaPadre(_, []) -> false;
estaPadre(Coord, [H | T]) ->
	Temp = lists:nth(6, H),
	if Temp == Coord ->
		true;
	true->
		estaPadre(Coord, T)
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
	Menor = menorFJP(Open),
	NuevaOpen = lists:delete(Menor, Open),
	Coordenadas = lists:nth(1, Menor),
	NuevaGlobal = cerrarNodo(Coordenadas, Global),
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

%Nodo con el menor F de la lista Open, similar al A*
menorFJP([]) -> [];
menorFJP(Open = [H | T]) -> 
	Fs = calcularFNodos([H | T]),
	Minimo = lists:min(Fs),
	I = indiceMenorF(Fs, Minimo, 1),
	lists:nth(I, Open).


%Calcula el Fs de los nodos en una lista
calcularFNodos([]) -> [];
calcularFNodos([H | T]) ->
	F = calcular_f(lists:nth(2, H) , lists:nth(3, H)),
	[F] ++ calcularFNodos(T).	


%Retorna el índice de un nodo en la lista
indiceMenorF([H|_], Min, I) when H == Min -> I;
indiceMenorF([_|T], Min, I) -> 
	IS = I + 1,
	indiceMenorF(T, Min, IS).

%Se marca un nodo como cerrado en la lista global
cerrarNodo(_, []) -> [];
cerrarNodo(Nodo, [H | T]) -> 
	Temporal = lists:nth(1, H),
	NodoG = lists:nth(2, H),
	NodoH = lists:nth(3, H),
	NodoListaOpen = lists:nth(4, H),
	Padre = lists:nth(6 , H),
	if Nodo /= Temporal -> 
		[H] ++ cerrarNodo(Nodo, T);
	true ->
		[[Nodo, NodoG, NodoH, NodoListaOpen, 1, Padre]]
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
				jp_algorithm_proc_vecinos(Neighbors, Global, Menor, Open, {Fx, Fy}, NodosVisitados)	
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

%Recorre la lista de vecinos, similar al de A*
jp_algorithm_proc_vecinos([Vecino | RestoVecinos], Global, Nodo =[{X, Y}, G , _], Open, {Fx, Fy}, NodosVisitados) -> 
	board ! {movejp, Vecino},
	NuevosVisitados = lists:append(NodosVisitados, [Vecino]),						
	Salto = saltar(Vecino, {X, Y}, {Fx, Fy}), %Verifica si se puede saltar o no a esa posición
	if Salto /= [] ->
		{SaltoX, SaltoY} = lists:nth(1, Salto),
		PuntoSalto = obtenerPuntoSalto(Global, {SaltoX, SaltoY}),
		if PuntoSalto == [] ->
			NodoSalto = [{SaltoX, SaltoY}, 0, 0, 0, 0, {X, Y}];
		true ->
			NodoSalto = PuntoSalto
		end,
		EstaClosed = estaClosed({SaltoX, SaltoX}, Global),
		if EstaClosed ->
				jp_algorithm_proc_vecinos(RestoVecinos, Global, Nodo, Open, {Fx, Fy}, NodosVisitados);
		true -> 
				Distancia = euclides({X, Y},{SaltoX, SaltoY}),
				NuevoG = G + Distancia,
				EstaOpen = estaOpen({SaltoX, SaltoY}, Global),
				ValorGPuntoSalto = lists:nth(2, NodoSalto),
				if (not EstaOpen) or (NuevoG < ValorGPuntoSalto) ->
						if (not EstaOpen) ->
							NuevoSaltoNodoL = [{SaltoX, SaltoY}, NuevoG, 
								euclides( {SaltoX, SaltoY}, {Fx, Fy} )],

							NuevoSaltoNodoG = [{SaltoX, SaltoY}, NuevoG, euclides( {SaltoX, SaltoY}, {Fx, Fy} ), 1, lists:nth(5, NodoSalto), {X, Y}],
							NuevaOpen = lists:append(Open, [NuevoSaltoNodoL]),
							NuevaGlobal = lists:append(Global, [NuevoSaltoNodoG]),
							board ! {movejp, {SaltoX, SaltoY}},
							[NuevaOpen, NuevaGlobal, lists:append(NuevosVisitados, [{SaltoX, SaltoY}])];
						true ->
							jp_algorithm_proc_vecinos(RestoVecinos, Global, Nodo, 
								Open, {Fx, Fy}, NodosVisitados)
						end;
				true->
						NuevaGlobal = lists:append(Global, [ValorGPuntoSalto]),
						jp_algorithm_proc_vecinos(RestoVecinos, NuevaGlobal, 
							Nodo, Open, {Fx, Fy}, NodosVisitados)
				end
		end;
	true ->
			jp_algorithm_proc_vecinos(RestoVecinos, Global, 
							Nodo, Open, {Fx, Fy}, NodosVisitados)
	end;
jp_algorithm_proc_vecinos([], Global, _, Open, _, NodosVisitados) -> [Open, Global, NodosVisitados].

%Se buscan estados closed en la lista
estaClosed(_, []) -> false;
estaClosed(Nodo, [H|T]) ->
	NodoTemp = lists:nth(1, H),
	if Nodo == NodoTemp -> 
		X = lists:nth(5, H),
		if X == 1 -> 
			true;
		true -> 
			false
		end;
	true ->
		estaClosed(Nodo, T) 
	end.

%Se buscan estados open en la lista
estaOpen(_, []) -> false;
estaOpen(Nodo, [H|T]) ->
	NodoTemp = lists:nth(1, H),
	if Nodo == NodoTemp -> 
		X = lists:nth(4, H),
		if X == 1 -> 
			true;
		true -> 
			false
		end;
	true ->
		estaOpen(Nodo, T) 
	end.

%Obtiene un nuevo punto de salto de acuerdo a las coordenadas que se pasan por parámetro
obtenerPuntoSalto([], _) -> [];
obtenerPuntoSalto([H | T], Nodo) -> 
	Coordenada = lists:nth(1, H),
	if Coordenada == Nodo -> 
			H;
	true ->
		obtenerPuntoSalto(T, Nodo)
	end.

%Verifica vertical,diagonal y horizontalmente para ver si el salto es válido y si hay vecinos forzados
saltar(Vecino = {X, Y}, {SaltoX, SaltoY}, Final)->
	DistanciaX = X - SaltoX,
	DistanciaY = Y - SaltoY,
	CuadroValido = posicionValida(X, Y),		
	if (not CuadroValido) ->
		[];
	Vecino == Final ->
		[{X, Y}];

	%Verificación de horizontales y verticales
	not ((DistanciaX /= 0) and (DistanciaY /= 0)) ->
		%Horizontalmente
		if (DistanciaX /= 0) ->
			HorizontalVertical = posicionValida((X + DistanciaX), (Y + 1)),
			Vertical = posicionValida(X, (Y + 1)),
			HorizontalVerticalAux = posicionValida((X + DistanciaX), (Y - 1)),
			VerticalAux = posicionValida(X, (Y - 1)),
			if (HorizontalVertical and (not Vertical)) or (HorizontalVerticalAux and (not VerticalAux)) ->
				[{X, Y}];
			true ->
				saltarAux(Vecino, {SaltoX, SaltoY}, Final)
			end;
		%Verticalmente
		true ->
			VerticalHorizontal  = posicionValida((X + 1), (Y + DistanciaY)),
			Vertical = posicionValida((X + 1), Y),
			VerticalHorizontalAux = posicionValida((X - 1), (Y + DistanciaY)),
			VerticalAux = posicionValida((X - 1), Y),
			if (VerticalHorizontal and (not Vertical)) or (VerticalHorizontalAux and (not VerticalAux)) ->
				[{X, Y}];
			true ->
				saltarAux(Vecino, {SaltoX, SaltoY}, Final)
			end
		end;
	%Diagonalmente
	(DistanciaX /= 0) and (DistanciaY /= 0) ->
		DiagonalNegativa = posicionValida((X - DistanciaX), (Y - DistanciaY)),
		DiagonalHorizontal = posicionValida((X - DistanciaX), Y),
		DiagonalPositiva = posicionValida((X + DistanciaX), (Y - DistanciaY)),
		DiagonalVertical = posicionValida(X , (Y - DistanciaY)),
		if (DiagonalNegativa and (not DiagonalHorizontal)) or (DiagonalPositiva and (not DiagonalVertical)) ->
			[{X, Y}];
		true ->
			saltarAux(Vecino, {SaltoX, SaltoY}, Final)
		end	
	end.

saltarAux({X, Y}, {SaltoX, SaltoY}, {FinalX, FinalY}) ->
	DistanciaX = X - SaltoX,
	DistanciaY = Y - SaltoY,
	if (DistanciaX /= 0) and (DistanciaY /= 0) ->
		NuevoX = saltar({(X + DistanciaX), Y}, {X, Y}, {FinalX, FinalY}),
		NuevoY = saltar({X, (Y + DistanciaY)}, {X, Y}, {FinalX, FinalY}),
		if (NuevoX /= []) or (NuevoY /= []) ->
				[{X, Y}];
		true ->
				saltarDeNuevo({X, Y}, {SaltoX, SaltoY}, {FinalX, FinalY})
		end;
	true ->
		saltarDeNuevo({X, Y}, {SaltoX, SaltoY}, {FinalX, FinalY})	
	end.

saltarDeNuevo({X, Y}, {SaltoX, SaltoY}, {FinalX, FinalY}) ->
	DistanciaX = X - SaltoX,
	DistanciaY = Y - SaltoY,				
	Horizontal = posicionValida((X + DistanciaX), Y),
	Vertical = posicionValida(X, (Y + DistanciaY)),
	if (Horizontal or Vertical) ->
		saltar({(X + DistanciaX), (Y + DistanciaY)}, {X, Y}, {FinalX, FinalY});
	true ->
		[]
	end.				


%Averigua si una posicion en el board NO es una pared
posicionValida(X, Y) ->
	board ! {get_all_board, self()},
	receive
	    Tablero ->
			Posicion = board:calcPos(X, Y),
				if Posicion == invalid -> 
					false;
			true ->
	    		case element(Posicion, Tablero) of
			        ?WALL ->
			            false;
			        _ ->
			            true
			    end
			end
	end.