module Robot
  ( isRobotAlive
  , updateRobotVelocity, updatePosition, botDecision
  , esEnemigo, robotsCercanos, aliadosCercanos
  , separarRobots
  ) where

import Entidades
import Fisicas
import Control.Applicative (liftA2)
import Data.List (find)

isRobotAlive :: Robot -> Bool
isRobotAlive r = energy (extras r) > 0

updateRobotVelocity :: Robot -> Velocity -> Robot
updateRobotVelocity r v = r { velocity = v }

updatePosition :: Robot -> Tiempo -> Robot
updatePosition r dt = 
  let newPos = position r ^+^ (velocity r ^* dt)
  in r { position = newPos }

-- Separar dos robots que están en colisión
separarRobots :: Robot -> Robot -> (Robot, Robot)
separarRobots r1 r2 =
  let p1 = position r1
      p2 = position r2
      delta = p1 ^-^ p2
      dist = sqrt (vx delta * vx delta + vy delta * vy delta)
      
      -- Distancia mínima basada en los tamaños
      V2 w1 h1 = size r1
      V2 w2 h2 = size r2
      minDist = max w1 w2 * 0.7 + 5  -- Radio + margen
      
      overlap = minDist - dist
  in if overlap > 0 && dist > 0.1
     then
       let direction = normalize delta
           -- Empujar cada robot la mitad de la distancia de overlap
           pushDist = (overlap / 2) + 2
           newPos1 = p1 ^+^ (direction ^* pushDist)
           newPos2 = p2 ^-^ (direction ^* pushDist)
       in (r1 { position = newPos1 }, r2 { position = newPos2 })
     else (r1, r2)

-- Verificar si dos robots son enemigos
esEnemigo :: Robot -> Robot -> Bool
esEnemigo r1 r2 = tipo (extras r1) /= tipo (extras r2)

smoothRotateTowards :: Angle -> Angle -> Float -> Angle
smoothRotateTowards current target speed =
  let diff = normalizeAngle (target - current)
      step = clamp (-speed) speed diff
  in current + step
  where
    normalizeAngle a
      | a > 180   = a - 360
      | a < -180  = a + 360
      | otherwise = a
    clamp lo hi v = max lo (min hi v)

--  : Verificar si un destino está en el historial de fallidos
estaEnHistorialFallido :: Position -> [Position] -> Float -> Bool
estaEnHistorialFallido destino historial radio =
  any (\pos -> distanceBetween destino pos < radio) historial

--  : Verificar si estamos dando vueltas en círculo
estaDandoVueltas :: [Position] -> Bool
estaDandoVueltas posiciones
  | length posiciones < 5 = False
  | otherwise =
      let ultimas5 = take 5 posiciones
          centro = promedioPosiciones ultimas5
          distancias = map (distanceBetween centro) ultimas5
          maxDist = maximum distancias
      in maxDist < 50  -- Si todas las últimas 5 posiciones están en un radio de 50, está dando vueltas

promedioPosiciones :: [Position] -> Position
promedioPosiciones [] = V2 0 0
promedioPosiciones ps =
  let n = fromIntegral (length ps)
      sumPos = foldl (^+^) (V2 0 0) ps
  in (1/n) *^ sumPos

--  : Generar destino que evite lugares ya intentados
generarDestinoNoRepetido :: Position -> [Position] -> [Position] -> Int -> Size -> Position
generarDestinoNoRepetido posActual historial fallidos tick (V2 wx wy) =
  let intentos = [0..20]  -- Intentar hasta 20 posiciones diferentes
      hash seed = (objectId posActual `mod` 1000 + seed * 37 + tick) `mod` 360
      generarCandidato seed =
        let angle = deg2rad (fromIntegral (hash seed))
            distancia = 200 + fromIntegral (seed * 30)
            candidato = posActual ^+^ V2 (cos angle * distancia) (sin angle * distancia)
            -- Mantener dentro de límites
            margen = 100
            V2 cx cy = candidato
            clamped = V2 (clamp margen (wx - margen) cx) (clamp margen (wy - margen) cy)
        in clamped
      
      -- Buscar primer candidato que no esté en historial ni fallidos
      buscarValido [] = generarCandidato 0  -- Si no encuentra, usar cualquiera
      buscarValido (seed:seeds) =
        let candidato = generarCandidato seed
            enHistorial = estaEnHistorialFallido candidato historial 40
            enFallidos = estaEnHistorialFallido candidato fallidos 60
        in if not enHistorial && not enFallidos
           then candidato
           else buscarValido seeds
  in buscarValido intentos
  where
    objectId (V2 x y) = floor (x + y) `mod` 1000
    clamp lo hi v = max lo (min hi v)

-- Detecta robots cercanos (todos)
robotsCercanos :: Robot -> [Robot] -> [Robot]
robotsCercanos self todos =
  let rango = range (extras self)
      pR = position self
  in filter (\e -> objectId e /= objectId self && 
                   distanceBetween pR (position e) <= rango) todos

-- Detecta solo enemigos en el radar
enemigosEnRadar :: Robot -> [Robot] -> [Robot]
enemigosEnRadar self todos =
  let rango = range (extras self)
      pR = position self
  in filter (\e -> esEnemigo self e && distanceBetween pR (position e) <= rango) todos

-- Detecta aliados cercanos (mismo tipo)
aliadosCercanos :: Robot -> [Robot] -> [Robot]
aliadosCercanos self todos =
  let rango = range (extras self)
      pR = position self
  in filter (\e -> not (esEnemigo self e) && 
                   objectId e /= objectId self && 
                   distanceBetween pR (position e) <= rango) todos

--  : Verificar si hay obstáculos bloqueando el camino
hayObstaculoEnCamino :: Position -> Position -> [Obstaculo] -> Bool
hayObstaculoEnCamino desde hacia obstaculos =
  let dir = normalize (hacia ^-^ desde)
      dist = distanceBetween desde hacia
  in any (\obs -> 
    let posObs = position obs
        V2 w h = size obs
        radioObs = max w h / 2 + 20  -- Radio del obstáculo + margen
        -- Verificar si el obstáculo está cerca de la línea de movimiento
        vectorAObs = posObs ^-^ desde
        proyeccion = vectorAObs `dot` dir
        -- Solo considerar obstáculos que están en la dirección del movimiento
        enDireccion = proyeccion > 0 && proyeccion < dist
        -- Distancia perpendicular del obstáculo a la línea
        distPerp = if enDireccion
                  then let puntoEnLinea = desde ^+^ (dir ^* proyeccion)
                       in distanceBetween posObs puntoEnLinea
                  else 999999
    in distPerp < radioObs
  ) obstaculos

--  : Encontrar una ruta alternativa evitando obstáculos
-- Parámetro prioridad: si es alta (ataque/huida urgente), puede ignorar obstáculos
evitarObstaculos :: Position -> Position -> [Obstaculo] -> Position
evitarObstaculos desde hacia obstaculos =
  if not (hayObstaculoEnCamino desde hacia obstaculos)
  then hacia
  else
    -- Buscar el obstáculo más cercano en el camino
    let dir = normalize (hacia ^-^ desde)
        obstaculosCerca = filter (\obs ->
          let posObs = position obs
              vectorAObs = posObs ^-^ desde
              proyeccion = vectorAObs `dot` dir
          in proyeccion > 0) obstaculos
    in case obstaculosCerca of
      [] -> hacia
      (obs:_) ->
        let posObs = position obs
            V2 w h = size obs
            radioEvasion = max w h / 2 + 60
            -- Calcular dirección perpendicular
            perpDir = perp dir
            -- Dos opciones: rodear por la izquierda o derecha
            opcion1 = posObs ^+^ (perpDir ^* radioEvasion)
            opcion2 = posObs ^-^ (perpDir ^* radioEvasion)
            dist1 = distanceBetween desde opcion1 + distanceBetween opcion1 hacia
            dist2 = distanceBetween desde opcion2 + distanceBetween opcion2 hacia
        in if dist1 < dist2 then opcion1 else opcion2

--  : Versión condicional - solo evita si no hay urgencia
evitarObstaculosCondicional :: Position -> Position -> [Obstaculo] -> Bool -> Position
evitarObstaculosCondicional desde hacia obstaculos ignorarSiUrgente =
  if ignorarSiUrgente 
  then hacia  -- En situaciones urgentes, ir directo aunque haya obstáculos
  else evitarObstaculos desde hacia obstaculos

--  : Verificar si hay obstáculos cerca de una posición (para tácticas)
hayObstaculoCerca :: Position -> [Obstaculo] -> Float -> Bool
hayObstaculoCerca pos obstaculos radio =
  any (\obs -> distanceBetween pos (position obs) < radio) obstaculos

--  : Intentar empujar al enemigo hacia un obstáculo
calcularPosicionTactica :: Position -> Position -> [Obstaculo] -> Maybe Position
calcularPosicionTactica posPropia posEnemigo obstaculos =
  let obstaculosCercanos = filter (\obs -> 
        let distAEnemigo = distanceBetween posEnemigo (position obs)
        in distAEnemigo < 200) obstaculos
  in case obstaculosCercanos of
    [] -> Nothing
    (obs:_) ->
      -- Posicionarse para empujar al enemigo hacia el obstáculo
      let posObs = position obs
          dirObsAEnemigo = normalize (posEnemigo ^-^ posObs)
          -- Posición táctica: entre el enemigo y el lado opuesto del obstáculo
          posTactica = posObs ^-^ (dirObsAEnemigo ^* 150)
      in Just posTactica

seleccionarObjetivoPrioritario :: Robot -> [Robot] -> Maybe Robot
seleccionarObjetivoPrioritario self [] = Nothing
seleccionarObjetivoPrioritario self enemigos =
  let pR = position self
      --  : Si hay un objetivo en memoria y no está estancado, intentar mantenerlo
      memT = memTarget (extras self)
      memStuck = memStuckCounter (extras self)
      
      -- Forzar cambio de objetivo solo si está MUY estancado
      debesCambiarObjetivo = memStuck > 220  
      
      objetivoActual = case memT of
        Just tid | not debesCambiarObjetivo -> find ((== tid) . objectId) enemigos
        _ -> Nothing
      
      scoreEnemigo e =
        let dist = distanceBetween pR (position e)
            vida = energy (extras e)
            vidaScore = vida * 2.0
            distScore = dist / 2.0
        in vidaScore + distScore
      mejorEnemigo = foldl1 (\e1 e2 -> if scoreEnemigo e1 < scoreEnemigo e2 then e1 else e2) enemigos
  in case objetivoActual of
       Just obj -> Just obj  -- Mantener objetivo actual si no está estancado
       Nothing -> Just mejorEnemigo  -- Buscar   objetivo

posicionExploracion :: Int -> Int -> Size -> Position -> Position
posicionExploracion robotId tick (V2 wx wy) currentPos =
  let 
    numSectoresX = 4
    numSectoresY = 3
    totalSectores = numSectoresX * numSectoresY
    
    cicloExploracion = (tick `div` 180) `mod` totalSectores
    sectorBase = (robotId * 7 + cicloExploracion) `mod` totalSectores
    
    sectorX = sectorBase `mod` numSectoresX
    sectorY = sectorBase `div` numSectoresX
    
    anchoSector = wx / fromIntegral numSectoresX
    altoSector = wy / fromIntegral numSectoresY
    
    margen = 80
    seed = robotId * 1000 + tick `div` 180
    
    hash n = let a = n * 15485863
                 b = (a `mod` 2038074743)
             in fromIntegral b / 2038074743.0
    
    offsetX = (hash seed - 0.5) * (anchoSector * 0.6)
    offsetY = (hash (seed + 1) - 0.5) * (altoSector * 0.6)
    
    baseX = fromIntegral sectorX * anchoSector + anchoSector / 2
    baseY = fromIntegral sectorY * altoSector + altoSector / 2
    
    targetX = clampVal margen (wx - margen) (baseX + offsetX)
    targetY = clampVal margen (wy - margen) (baseY + offsetY)
    
  in V2 targetX targetY
  where
    clampVal lo hi v = max lo (min hi v)

botDecision :: Int -> GameState -> [Obstaculo] -> Robot -> [Robot] -> [BotAction]
botDecision tick gs obstaculos self others =
  let enemigos = enemigosEnRadar self others
  in case tipo (extras self) of
       Humano  -> estrategiaHumano tick gs obstaculos self enemigos
       Zombie  -> estrategiaZombie tick gs obstaculos self enemigos

-- HUMANOS: Disparan a los zombies
estrategiaHumano :: Int -> GameState -> [Obstaculo] -> Robot -> [Robot] -> [BotAction]
estrategiaHumano tick gs obstaculos r enemigosRadar =
  let
    pR = position r
    V2 wx wy = worldSize gs
    margen = 100
    cercaBorde = vx pR < margen || vx pR > wx - margen || 
                 vy pR < margen || vy pR > wy - margen
    centro = V2 (wx / 2) (wy / 2)
    
    currentCannonAngle = anguloCanon r
    vida = energy (extras r)
    rango = range (extras r)
    
    -- MEMORIA: Recuperar estado previo
    memT = memTarget (extras r)
    memR = memRole (extras r)
    memLast = memLastSeen (extras r)
    memCooldown = memAggroCooldown (extras r)
    memLastPos = memLastPosition (extras r)
    memStuck = memStuckCounter (extras r)
    memLastDir = memLastMoveDir (extras r)
    memHistory = memPositionHistory (extras r)  --  
    memFailed = memFailedDestinations (extras r)  --  
    
    --  : Detección de estancamiento (MUY SUAVE - evitar temblor)
    distanciaMovimiento = case memLastPos of
      Just lastPos -> distanceBetween pR lastPos
      Nothing -> 999
    
    estaEstancado = distanciaMovimiento < 1.5  -- Reducido de 3.0 a 1.5 - REALMENTE atascado
    nuevoStuckCounter = if estaEstancado then memStuck + 1 else max 0 (memStuck - 3)  -- Decrece aún más rápido
    
    --  : Detectar si está dando vueltas en círculo
    dandoVueltas = estaDandoVueltas memHistory
    
    -- Si lleva MUCHO tiempo estancado (150 frames = 2.5 segundos), cambiar dirección - Aumentado de 90 a 150
    debeReorientar = nuevoStuckCounter > 150 || (dandoVueltas && nuevoStuckCounter > 100)
    
    aliados = aliadosCercanos r enemigosRadar
    distanciaMinAliados = 100
    distanciaOptima = 150
    
    centroGrupo = if null aliados 
                  then pR
                  else let sumPos = foldl (\acc a -> acc ^+^ position a) pR aliados
                       in (1 / fromIntegral (length aliados + 1)) *^ sumPos
    
    -- SISTEMA DE ROLES TÁCTICOS DINÁMICO
    miId = objectId r
    esLider = miId == minimum (miId : map objectId aliados)
    
    rolActual = case memR of
      Just "sniper" -> "sniper"
      Just "tank" -> "tank"
      Just "support" -> "support"
      _ -> if esLider then "tank" 
           else if vida > 70 then "sniper"
           else "support"
    
    debeActualizarRol = tick `mod` 120 == 0 || 
                       (vida < 30 && rolActual /= "support")
    
    nuevoRol = if debeActualizarRol
               then if vida < 30 then "support"
                    else if esLider then "tank"
                    else if length aliados < 2 then "tank"
                    else if vida > 70 && length enemigosRadar > 0 then "sniper"
                    else rolActual
               else rolActual
    
    accionRol = if nuevoRol /= rolActual 
                then [SetRole (Just nuevoRol)]
                else []
    
    (distanciaPreferida, cadenciaDisparo, velocidadRotacion) = case nuevoRol of
      "tank"    -> (rango * 0.60, 40, 2.0)  
      "sniper"  -> (rango * 0.85, 60, 1.5)  
      "support" -> (rango * 0.70, 50, 2.0)  
      _         -> (rango * 0.75, 55, 2.0) 
    
    aliadosHeridos = filter (\a -> energy (extras a) < 50) aliados
    
    necesitaCobertura = case aliadosHeridos of
      (herido:_) -> 
        let distHerido = distanceBetween pR (position herido)
        in distHerido < 200 && vida > 60
      _ -> False
    
    ajustarFormacionDefensiva destino =
      let numAliados = length aliados
          miIndice = length (filter (\a -> objectId a < miId) aliados)
          anguloFormacion = (360 / fromIntegral (numAliados + 1)) * fromIntegral miIndice
          
          radioFormacion = case nuevoRol of
            "tank" -> 120
            "sniper" -> 200
            "support" -> 160
            _ -> 150
          
          offsetFormacion = V2 
            (cos (deg2rad anguloFormacion) * radioFormacion)
            (sin (deg2rad anguloFormacion) * radioFormacion)
          
          puntoFormacion = centroGrupo ^+^ offsetFormacion
          
          fuerzaRepulsion = foldl calcularRepulsion (pure 0) aliados
          calcularRepulsion acc aliado =
            let dist = distanceBetween pR (position aliado)
                dir = normalize (pR ^-^ position aliado)
                fuerza = if dist < distanciaMinAliados
                        then 300 * (distanciaMinAliados / max 1 dist)
                        else if dist < distanciaOptima
                             then 100 * (distanciaOptima - dist) / distanciaOptima
                             else 0
            in acc ^+^ (dir ^* fuerza)
          
          direccionFinal = if null aliados
                          then normalize (destino ^-^ pR)
                          else normalize ((puntoFormacion ^-^ pR) ^+^ fuerzaRepulsion)
      in pR ^+^ (direccionFinal ^* 60)
    
  in case seleccionarObjetivoPrioritario r enemigosRadar of
    Nothing ->
      -- SIN ENEMIGOS
      let 
          nuevoCooldown = max 0 (memCooldown - 1)
          
          debeInvestigar = case (memT, memLast) of
            (Just _, Just lastPos) -> 
              nuevoCooldown > 0 && distanceBetween pR lastPos > 50
            _ -> False
          
       
          direccionAleatoria = 
            if dandoVueltas || nuevoStuckCounter > 140  
            then generarDestinoNoRepetido pR memHistory memFailed tick (worldSize gs)
            else 
              let hash = (objectId r * 1000 + tick) `mod` 360
                  angle = deg2rad (fromIntegral hash)
                  distancia = 250
              in pR ^+^ V2 (cos angle * distancia) (sin angle * distancia)
          
          destinoPatrulla = case nuevoRol of
            "tank" -> 
              let angulo = fromIntegral tick * 0.008
                  radio = min wx wy * 0.4
              in centro ^+^ V2 (cos angulo * radio) (sin angulo * radio)
            
            "sniper" ->
              let esquina = tick `div` 300 `mod` 4
                  offset = 150
              in case esquina of
                0 -> V2 offset offset
                1 -> V2 (wx - offset) offset
                2 -> V2 (wx - offset) (wy - offset)
                _ -> V2 offset (wy - offset)
            
            "support" -> centroGrupo
            _ -> posicionExploracion miId tick (worldSize gs) pR
          
          destinoFinal = if cercaBorde then centro
                       else if debeReorientar then direccionAleatoria  --  : Si estancado, ir en dirección aleatoria
                       else if debeInvestigar 
                            then maybe destinoPatrulla id memLast
                       else destinoPatrulla
          
          --  : Evitar obstáculos
          destinoSinObstaculos = evitarObstaculos pR destinoFinal obstaculos
          destinoConFormacion = ajustarFormacionDefensiva destinoSinObstaculos
          
          barrido = sin (fromIntegral tick * 0.04) * 60
          targetAngle = rad2deg (angleToTarget pR destinoConFormacion)
          newCannonAngle = smoothRotateTowards currentCannonAngle 
                                               (targetAngle + barrido) 
                                               velocidadRotacion
          
          accionesMemoria = [SetAggroCooldown nuevoCooldown]
          accionesEstancamiento = [UpdateStuckState pR nuevoStuckCounter (Just (normalize (destinoConFormacion ^-^ pR)))]
          accionesHistorial = [UpdatePositionHistory pR]  
          accionesFallido = if nuevoStuckCounter > 180 then [MarkFailedDestination destinoConFormacion] else []  -- Aumentado de 120 a 180
          
      in accionRol ++ accionesMemoria ++ accionesEstancamiento ++ accionesHistorial ++ accionesFallido ++
         [RotateCannon (deg2rad newCannonAngle), Move destinoConFormacion]
    
    Just zombie ->
      let
        pZ = position zombie
        idZ = objectId zombie
        dist = distanceBetween pR pZ
        
        accionesMemoria = 
          [ SetTarget (Just idZ)
          , UpdateLastSeen pZ
          , SetAggroCooldown 180
          ]
        
        --  : Si está muy estancado, limpiar objetivo para forzar cambio
        accionesMemoriaFinal = if shouldForceNewTarget
                              then [SetTarget Nothing, SetAggroCooldown 0]
                              else accionesMemoria
        
        targetAngle = rad2deg (angleToTarget pR pZ)
        newCannonAngle = smoothRotateTowards currentCannonAngle targetAngle velocidadRotacion
        
        dir = normalize (pZ ^-^ pR)
        dirHuida = normalize (pR ^-^ pZ)
        canionDir = let t = deg2rad newCannonAngle in V2 (cos t) (sin t)
        estoyApuntando = canionDir `dot` dir > 0.92
        
        targetFinal = if necesitaCobertura && not (null aliadosHeridos)
                      then 
                        let herido = head aliadosHeridos
                            posHerido = position herido
                        in if distanceBetween posHerido pZ < 150
                           then pZ
                           else pZ
                      else pZ
        
        zonaEstable = 25
        
        (minSafe, optDist, maxDist) = case nuevoRol of
          "tank"    -> (distanciaPreferida - 50, distanciaPreferida, distanciaPreferida + 30)
          "sniper"  -> (distanciaPreferida - 30, distanciaPreferida, distanciaPreferida + 20)
          "support" -> (distanciaPreferida - 40, distanciaPreferida, distanciaPreferida + 40)
          _         -> (140, distanciaPreferida, rango * 0.95)
        
        puedeDisparar = tick `mod` cadenciaDisparo == 0
        
        --  : Determinar si hay urgencia (huida por baja vida o ataque agresivo)
        vidaBaja = vida < 40
        muyPeligroso = dist < 80
        urgenciaHuida = vidaBaja && muyPeligroso
        urgenciaAtaque = dist < 150 && vida > 100 && nuevoRol == "tank"
        
        --  : Táctica - intentar empujar al zombie hacia obstáculos
        posicionTacticaObstaculo = calcularPosicionTactica pR pZ obstaculos
        
        --  : Si está estancado en combate, usar evasión más agresiva
        direccionEvasion = if debeReorientar
          then let hash = (objectId r * 1000 + tick) `mod` 360
                   angle = deg2rad (fromIntegral hash)
                   -- MOVIMIENTO MÁS GRANDE para salir del atasco
                   distancia = 180
               in pR ^+^ V2 (cos angle * distancia) (sin angle * distancia)
          else pZ
        
        --  : Forzar cambio de objetivo si está muy estancado
        shouldForceNewTarget = debeReorientar && nuevoStuckCounter > 200  -- Aumentado de 150 a 200
        
        destino =
          if cercaBorde then centro
          else if urgenciaHuida then
            -- HUIDA URGENTE: ignorar obstáculos, huir directo
            pR ^+^ dirHuida ^* 120
          else if debeReorientar && vida < 50 then
            --  : Si está estancado y con poca vida, evadir agresivamente
            direccionEvasion
          else if necesitaCobertura && not (null aliadosHeridos) then
            let herido = head aliadosHeridos
                posHerido = position herido
                dirProteccion = normalize (pZ ^-^ posHerido)
            in posHerido ^+^ (dirProteccion ^* 100)
          else if vida < 30 && nuevoRol /= "tank" then
            pR ^+^ dirHuida ^* 100
          else if dist < (minSafe - zonaEstable) then
            pR ^+^ dirHuida ^* 80
          else if dist > (maxDist + zonaEstable) then
            -- Avanzar hacia el enemigo, pero usar táctica si hay obstáculos cerca
            case posicionTacticaObstaculo of
              Just posTactica | urgenciaAtaque -> posTactica  -- Empujar hacia obstáculo
              _ -> pR ^+^ dir ^* 70
          else
            let perpDir = perp dir
                strafeDir = if tick `mod` 200 < 100 then perpDir else perpDir ^* (-1)
            in pR ^+^ (strafeDir ^* 40)
        
        --  : Evitar obstáculos solo si NO hay urgencia
        destinoSinObstaculos = evitarObstaculosCondicional pR destino obstaculos urgenciaHuida
        destinoFinal = ajustarFormacionDefensiva destinoSinObstaculos
        
        accionDisparo = if estoyApuntando && puedeDisparar 
                        then [Shoot] 
                        else []
        
        accionesEstancamiento = [UpdateStuckState pR nuevoStuckCounter (Just dir)]
        accionesHistorial = [UpdatePositionHistory pR]  --  
        accionesFallido = if shouldForceNewTarget then [MarkFailedDestination destinoFinal] else []  --  
        
      in accionRol ++ accionesMemoriaFinal ++ accionesEstancamiento ++ accionesHistorial ++ accionesFallido ++
         [RotateCannon (deg2rad newCannonAngle), Move destinoFinal] ++ 
         accionDisparo

-- ZOMBIES: Persiguen y atacan a los humanos
estrategiaZombie :: Int -> GameState -> [Obstaculo] -> Robot -> [Robot] -> [BotAction]
estrategiaZombie tick gs obstaculos r enemigosRadar =
  let
    pR = position r
    V2 wx wy = worldSize gs
    margen = 100
    cercaBorde = vx pR < margen || vx pR > wx - margen || 
                 vy pR < margen || vy pR > wy - margen
    centro = V2 (wx / 2) (wy / 2)
    
    currentAngle = angulo r
    
    memLast = memLastSeen (extras r)
    memCooldown = memAggroCooldown (extras r)
    memLastPos = memLastPosition (extras r)
    memStuck = memStuckCounter (extras r)
    memLastDir = memLastMoveDir (extras r)
    memHistory = memPositionHistory (extras r)  --  
    memFailed = memFailedDestinations (extras r)  --  
    
    --  : Detección de estancamiento (MUY SUAVE - evitar temblor)
    distanciaMovimiento = case memLastPos of
      Just lastPos -> distanceBetween pR lastPos
      Nothing -> 999
    
    estaEstancado = distanciaMovimiento < 1.5  -- REALMENTE atascado
    nuevoStuckCounter = if estaEstancado then memStuck + 1 else max 0 (memStuck - 3)
    dandoVueltas = estaDandoVueltas memHistory  --  
    debeReorientar = nuevoStuckCounter > 150 || (dandoVueltas && nuevoStuckCounter > 100)  -- Aumentado tiempo
    
    aliados = aliadosCercanos r enemigosRadar
    distanciaMinAliados = 80
    
    velocidadRotacion = 2.0  -- Reducido de 3.5 a 2.0 para rotación muy suave
  
  in case seleccionarObjetivoPrioritario r enemigosRadar of
    Nothing ->
      let 
          nuevoCooldown = max 0 (memCooldown - 1)
          debeInvestigar = nuevoCooldown > 30 && memLast /= Nothing
          
          --  : Dirección aleatoria que evita lugares ya intentados
          direccionAleatoria = 
            if dandoVueltas || nuevoStuckCounter > 140  -- Aumentado de 100 a 140
            then generarDestinoNoRepetido pR memHistory memFailed tick (worldSize gs)
            else
              let hash = (objectId r * 2000 + tick) `mod` 360
                  angle = deg2rad (fromIntegral hash)
                  distancia = 250
              in pR ^+^ V2 (cos angle * distancia) (sin angle * distancia)
          
          destinoBase = case memLast of
            Just lastPos | debeInvestigar && distanceBetween pR lastPos > 50 -> lastPos
            _ -> 
              let angulo = fromIntegral (tick + objectId r * 100) * 0.01
                  radio = min wx wy * 0.35
              in centro ^+^ V2 (cos angulo * radio) (sin angulo * radio)
          
          destinoFinal = if cercaBorde then centro 
                       else if debeReorientar then direccionAleatoria  --  : Si estancado, ir en dirección aleatoria
                       else destinoBase
          
          fuerzaRepulsion = foldl (\acc a -> 
            let dist = distanceBetween pR (position a)
                dir = normalize (pR ^-^ position a)
            in if dist < distanciaMinAliados 
               then acc ^+^ (dir ^* 150)
               else acc
            ) (pure 0) aliados
          
          --  : Evitar obstáculos
          destinoSinObstaculos = evitarObstaculos pR destinoFinal obstaculos
          destinoConRepulsion = destinoSinObstaculos ^+^ fuerzaRepulsion
          
          targetAngle = rad2deg (angleToTarget pR destinoConRepulsion)
          newAngle = smoothRotateTowards currentAngle targetAngle velocidadRotacion
          
          accionesMemoria = if nuevoCooldown == 0
                           then [SetTarget Nothing, SetAggroCooldown 0]
                           else [SetAggroCooldown nuevoCooldown]
          
          accionesEstancamiento = [UpdateStuckState pR nuevoStuckCounter (Just (normalize (destinoConRepulsion ^-^ pR)))]
          accionesHistorial = [UpdatePositionHistory pR]  --  
          accionesFallido = if nuevoStuckCounter > 180 then [MarkFailedDestination destinoConRepulsion] else []  -- Aumentado de 120 a 180
          
      in accionesMemoria ++ accionesEstancamiento ++ accionesHistorial ++ accionesFallido ++ [Rotate (deg2rad newAngle), Move destinoConRepulsion]
    
    Just humano ->
      let
        pH = position humano
        idH = objectId humano
        dist = distanceBetween pR pH
        
        accionesMemoria = 
          [ SetTarget (Just idH)
          , UpdateLastSeen pH
          , SetAggroCooldown 120
          ]
        
        --  : Si está muy estancado, limpiar objetivo para forzar cambio
        accionesMemoriaFinal = if shouldForceNewTarget
                              then [SetTarget Nothing, SetAggroCooldown 0]
                              else accionesMemoria
        
        dir = normalize (pH ^-^ pR)
        
        fuerzaRepulsion = foldl (\acc a -> 
          let dist' = distanceBetween pR (position a)
              dir' = normalize (pR ^-^ position a)
          in if dist' < distanciaMinAliados 
             then acc ^+^ (dir' ^* 120)
             else acc
          ) (pure 0) aliados
        
        distanciaAtaque = 60
        vidaZombie = energy (extras r)
        
        --  : Determinar urgencia para zombies
        muyHerido = vidaZombie < 50
        muyCerca = dist < 100
        urgenciaAtaque = muyCerca || (dist < 150 && vidaZombie > 150)
        
        --  : Táctica zombi - empujar humano hacia obstáculos
        posicionTacticaObstaculo = calcularPosicionTactica pR pH obstaculos
        
        --  : Si está estancado, usar ataque más directo
        direccionDirecta = if debeReorientar
          then pH  -- Ir directo al objetivo si está estancado
          else pH
        
        --  : Forzar cambio de objetivo si está muy estancado
        shouldForceNewTarget = debeReorientar && nuevoStuckCounter > 200  -- Aumentado de 150 a 200
        
        destino = if cercaBorde 
                 then centro
                 else if debeReorientar && dist > distanciaAtaque then
                   --  : Si está estancado, ataque más directo
                   direccionDirecta
                 else if urgenciaAtaque && dist > distanciaAtaque then
                   -- Ataque agresivo: usar táctica de obstáculo si está disponible
                   case posicionTacticaObstaculo of
                     Just posTactica -> posTactica
                     Nothing -> pH
                 else if dist > distanciaAtaque 
                      then pH
                      else pR ^+^ (dir ^* 25)
        
        --  : Evitar obstáculos solo si no está atacando agresivamente o muy herido
        ignorarObstaculos = urgenciaAtaque || muyHerido
        destinoSinObstaculos = evitarObstaculosCondicional pR destino obstaculos ignorarObstaculos
        destinoFinal = destinoSinObstaculos ^+^ (fuerzaRepulsion ^* 0.4)
        
        targetAngle = rad2deg (angleToTarget pR destinoFinal)
        newAngle = smoothRotateTowards currentAngle targetAngle velocidadRotacion
        
        --  : Zombies disparan cuando están cerca
        apuntaBien = abs (currentAngle - targetAngle) < 15
        puedeDisparar = dist < 200 && apuntaBien
        accionesAtaque = if puedeDisparar then [Shoot] else []
        
        accionesEstancamiento = [UpdateStuckState pR nuevoStuckCounter (Just dir)]
        accionesHistorial = [UpdatePositionHistory pR]  --  
        accionesFallido = if shouldForceNewTarget then [MarkFailedDestination destinoFinal] else []  --  
        
      in accionesMemoriaFinal ++ accionesEstancamiento ++ accionesHistorial ++ accionesFallido ++ [Rotate (deg2rad newAngle), Move destinoFinal] ++ accionesAtaque
