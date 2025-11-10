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


separarRobots :: Robot -> Robot -> (Robot, Robot)
separarRobots r1 r2 =
  let p1 = position r1
      p2 = position r2
      delta = p1 ^-^ p2
      dist = sqrt (vx delta * vx delta + vy delta * vy delta)
      
      V2 w1 h1 = size r1
      V2 w2 h2 = size r2
      minDist = (max w1 w2 + max h1 h2) * 0.5  
      
      overlap = minDist - dist
  in if overlap > 0 && dist > 0.1
     then
       let direction = normalize delta

           pushDist = overlap * 0.3  
           newPos1 = p1 ^+^ (direction ^* pushDist)
           newPos2 = p2 ^-^ (direction ^* pushDist)
       in (r1 { position = newPos1 }, r2 { position = newPos2 })
     else (r1, r2)


esEnemigo :: Robot -> Robot -> Bool
esEnemigo r1 r2 = tipo (extras r1) /= tipo (extras r2)

-- Rotación suave con inercia para evitar temblores
smoothRotateTowards :: Angle -> Angle -> Float -> Angle
smoothRotateTowards current target speed =
  let diff = normalizeAngle (target - current)
      distToTarget = abs diff
 
      easedSpeed = if distToTarget < 15 
                   then speed * (distToTarget / 15)
                   else speed
      step = clamp (-easedSpeed) easedSpeed diff
  in current + step
  where
    normalizeAngle a
      | a > 180   = a - 360
      | a < -180  = a + 360
      | otherwise = a
    clamp lo hi v = max lo (min hi v)


estaEnHistorialFallido :: Position -> [Position] -> Float -> Bool
estaEnHistorialFallido destino historial radio =
  any (\pos -> distanceBetween destino pos < radio) historial


estaDandoVueltas :: [Position] -> Bool
estaDandoVueltas posiciones
  | length posiciones < 5 = False
  | otherwise =
      let ultimas5 = take 5 posiciones
          centro = promedioPosiciones ultimas5
          distancias = map (distanceBetween centro) ultimas5
          maxDist = maximum distancias
      in maxDist < 50  

promedioPosiciones :: [Position] -> Position
promedioPosiciones [] = V2 0 0
promedioPosiciones ps =
  let n = fromIntegral (length ps)
      sumPos = foldl (^+^) (V2 0 0) ps
  in (1/n) *^ sumPos


generarDestinoNoRepetido :: Position -> [Position] -> [Position] -> Int -> Size -> Position
generarDestinoNoRepetido posActual historial fallidos tick (V2 wx wy) =
  let intentos = [0..20]  
      hash seed = (objectId posActual `mod` 1000 + seed * 37 + tick) `mod` 360
      generarCandidato seed =
        let angle = deg2rad (fromIntegral (hash seed))
            distancia = 200 + fromIntegral (seed * 30)
            candidato = posActual ^+^ V2 (cos angle * distancia) (sin angle * distancia)
      
            margen = 100
            V2 cx cy = candidato
            clamped = V2 (clamp margen (wx - margen) cx) (clamp margen (wy - margen) cy)
        in clamped
      

      buscarValido [] = generarCandidato 0 
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

-- Detecta robots cercanos 
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

-- Detecta aliados cercanos 
aliadosCercanos :: Robot -> [Robot] -> [Robot]
aliadosCercanos self todos =
  let rango = range (extras self)
      pR = position self
  in filter (\e -> not (esEnemigo self e) && 
                   objectId e /= objectId self && 
                   distanceBetween pR (position e) <= rango) todos


hayObstaculoEnCamino :: Position -> Position -> [Obstaculo] -> Bool
hayObstaculoEnCamino desde hacia obstaculos =
  let dir = normalize (hacia ^-^ desde)
      dist = distanceBetween desde hacia
  in any (\obs -> 
    let posObs = position obs
        V2 w h = size obs
        radioObs = max w h / 2 + 5  

        vectorAObs = posObs ^-^ desde
        proyeccion = vectorAObs `dot` dir
   
        enDireccion = proyeccion > 0 && proyeccion < dist

        distPerp = if enDireccion
                  then let puntoEnLinea = desde ^+^ (dir ^* proyeccion)
                       in distanceBetween posObs puntoEnLinea
                  else 999999
    in distPerp < radioObs
  ) obstaculos

evitarObstaculos :: Position -> Position -> [Obstaculo] -> Position
evitarObstaculos desde hacia obstaculos =
  if not (hayObstaculoEnCamino desde hacia obstaculos)
  then hacia
  else
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
            radioEvasion = max w h / 2 + 30
            perpDir = perp dir
            opcion1 = posObs ^+^ (perpDir ^* radioEvasion)
            opcion2 = posObs ^-^ (perpDir ^* radioEvasion)
            dist1 = distanceBetween desde opcion1 + distanceBetween opcion1 hacia
            dist2 = distanceBetween desde opcion2 + distanceBetween opcion2 hacia
        in if dist1 < dist2 then opcion1 else opcion2


encontrarRutaRodeando :: Position -> Position -> [Obstaculo] -> Position
encontrarRutaRodeando desde hacia obstaculos =
  if not (hayObstaculoEnCamino desde hacia obstaculos)
  then hacia 
  else

    let dir = normalize (hacia ^-^ desde)
        distTotal = distanceBetween desde hacia
        obstaculosBloqueantes = filter (\obs ->
          let posObs = position obs
              V2 w h = size obs
              radioObs = max w h / 2 + 15
              vectorAObs = posObs ^-^ desde
              proyeccion = vectorAObs `dot` dir
              enDireccion = proyeccion > 0 && proyeccion < distTotal
              distPerp = if enDireccion
                        then let puntoEnLinea = desde ^+^ (dir ^* proyeccion)
                             in distanceBetween posObs puntoEnLinea
                        else 999999
          in distPerp < radioObs) obstaculos
    in case obstaculosBloqueantes of
      [] -> hacia
      _ ->
        -- Encontrar el obstáculo más cercano
        let obsMasCercano = foldl1 (\obs1 obs2 ->
              if distanceBetween desde (position obs1) < distanceBetween desde (position obs2)
              then obs1 else obs2) obstaculosBloqueantes
            posObs = position obsMasCercano
            V2 w h = size obsMasCercano
            radioEvasion = max w h / 2 + 50  
            
            perpDir = perp dir

            opcion1 = posObs ^+^ (perpDir ^* radioEvasion)
            opcion2 = posObs ^-^ (perpDir ^* radioEvasion)
            opcion3 = posObs ^+^ (perpDir ^* (radioEvasion * 0.7))
            opcion4 = posObs ^-^ (perpDir ^* (radioEvasion * 0.7))
            
  
            evaluarOpcion opt =
              let distancia = distanceBetween desde opt + distanceBetween opt hacia
                  hayObstaculo = hayObstaculoEnCamino desde opt obstaculos
                  penalizacion = if hayObstaculo then 500 else 0
              in distancia + penalizacion
            
            opciones = [(opcion1, evaluarOpcion opcion1),
                       (opcion2, evaluarOpcion opcion2),
                       (opcion3, evaluarOpcion opcion3),
                       (opcion4, evaluarOpcion opcion4)]
            
            mejorOpcion = fst $ foldl1 (\(p1, c1) (p2, c2) -> if c1 < c2 then (p1, c1) else (p2, c2)) opciones
            
            
            waypointBloqueado = hayObstaculoEnCamino desde mejorOpcion obstaculos
        in if waypointBloqueado
           then desde ^+^ (dir ^* 50)
           else mejorOpcion


calcularPosicionTactica :: Position -> Position -> [Obstaculo] -> Maybe Position
calcularPosicionTactica posPropia posEnemigo obstaculos =
  let obstaculosCercanos = filter (\obs -> 
        let distAEnemigo = distanceBetween posEnemigo (position obs)
        in distAEnemigo < 200) obstaculos
  in case obstaculosCercanos of
    [] -> Nothing
    (obs:_) ->

      let posObs = position obs
          dirObsAEnemigo = normalize (posEnemigo ^-^ posObs)
          posTactica = posObs ^-^ (dirObsAEnemigo ^* 150)
      in Just posTactica

seleccionarObjetivoPrioritario :: Robot -> [Robot] -> Maybe Robot
seleccionarObjetivoPrioritario self [] = Nothing
seleccionarObjetivoPrioritario self enemigos =
  let pR = position self
      memT = memTarget (extras self)
      memStuck = memStuckCounter (extras self)
      
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
       Just obj -> Just obj  
       Nothing -> Just mejorEnemigo  

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

    currentBodyAngle   = angulo r
    currentCannonAngle = anguloCanon r

    vida  = energy (extras r)
    rango = range  (extras r)


    aliados = aliadosCercanos r enemigosRadar

    miId = objectId r
    variacionIndividual  = fromIntegral (miId `mod` 60)
    distanciaMinAliados  = 100 + variacionIndividual  
    distanciaOptima      = 150 + variacionIndividual * 1.5  

    centroGrupo =
      if null aliados
        then pR
        else let sumPos = foldl (\acc a -> acc ^+^ position a) pR aliados
             in (1 / fromIntegral (length aliados + 1)) *^ sumPos

    -- ========= ROLES =========
    esLider      = miId == minimum (miId : map objectId aliados)
    personalidad = miId `mod` 5

    rolActual =
      if esLider then "tank" else
      case personalidad of
        0 -> if vida > 70 then "sniper" else "support"
        1 -> "flanker"
        2 -> "tank"
        3 -> "scout"
        _ -> "support"

    accionRol = []

    (distanciaPreferida, cadenciaDisparo, velocidadRotacion) =
      case rolActual of
        "tank"    -> (rango * 0.60, 40, 2.0)
        "sniper"  -> (rango * 0.85, 60, 1.5)
        "support" -> (rango * 0.70, 50, 2.0)
        "flanker" -> (rango * 0.65, 45, 2.2)
        "scout"   -> (rango * 0.80, 55, 2.5)
        _         -> (rango * 0.75, 55, 2.0)

    aliadosHeridos = filter (\a -> energy (extras a) < 50) aliados
    necesitaCobertura =
      case aliadosHeridos of
        (herido:_) ->
          let distHerido = distanceBetween pR (position herido)
          in distHerido < 200 && vida > 60
        _ -> False

    ajustarFormacionDefensiva destino =
      let numAliados = length aliados
          miIndice   = length (filter (\a -> objectId a < miId) aliados)
          offsetAngular    = fromIntegral ((miId * 37) `mod` 360)
          anguloFormacion  = offsetAngular + (360 / fromIntegral (numAliados + 1)) * fromIntegral miIndice
          radioBase = case rolActual of
            "tank"    -> 120
            "sniper"  -> 220
            "support" -> 160
            "flanker" -> 180
            "scout"   -> 250
            _         -> 150
          variacionRadio = fromIntegral ((miId * 17) `mod` 60) - 30
          radioFormacion = radioBase + variacionRadio
          offsetFormacion = V2 (cos (deg2rad anguloFormacion) * radioFormacion)
                               (sin (deg2rad anguloFormacion) * radioFormacion)
          puntoFormacion = centroGrupo ^+^ offsetFormacion

          fuerzaRepulsion = foldl calcularRepulsion (pure 0) aliados
          calcularRepulsion acc aliado =
            let dist = distanceBetween pR (position aliado)
                dir  = normalize (pR ^-^ position aliado)
                fuerza =
                  if dist < distanciaMinAliados
                    then 40 * ((distanciaMinAliados - dist) / distanciaMinAliados)
                  else if dist < distanciaOptima
                    then 15 * ((distanciaOptima - dist) / distanciaOptima)
                  else 0
            in acc ^+^ (dir ^* fuerza)

          pesoFormacionBase = case rolActual of
            "tank"    -> 0.2
            "sniper"  -> 0.05
            "support" -> 0.15
            "flanker" -> 0.08
            "scout"   -> 0.02
            _         -> 0.1

          pesoFormacion = if numAliados < 2 then pesoFormacionBase * 0.5 else pesoFormacionBase
          direccionDestino   = normalize (destino ^-^ pR)
          direccionFormacion = normalize ((puntoFormacion ^-^ pR) ^+^ fuerzaRepulsion)
          direccionFinal =
            if null aliados
              then direccionDestino
              else normalize (direccionDestino ^* (1.0 - pesoFormacion) ^+^ direccionFormacion ^* pesoFormacion)
      in pR ^+^ (direccionFinal ^* 55)

  in case seleccionarObjetivoPrioritario r enemigosRadar of
    Nothing ->
      let
        destinoPatrulla = case rolActual of
          "tank" ->
            let angulo = fromIntegral (tick `div` 200 + miId * 13) * 0.5
                radio  = min wx wy * 0.4
            in  centro ^+^ V2 (cos angulo * radio) (sin angulo * radio)
          "sniper" ->
            let esquina = (tick `div` 500 + miId) `mod` 4
                offset  = 150 + fromIntegral (miId `mod` 50)
            in case esquina of
                 0 -> V2 offset offset
                 1 -> V2 (wx - offset) offset
                 2 -> V2 (wx - offset) (wy - offset)
                 _ -> V2 offset (wy - offset)
          "support" ->
            let base = if null aliados then centro else centroGrupo
            in  base
          "flanker" ->
            let angulo = fromIntegral (tick `div` 180 + miId * 27) * 0.6
                radio  = min wx wy * 0.42
            in  centro ^+^ V2 (cos angulo * radio) (sin angulo * radio)
          "scout" ->
            let angulo = fromIntegral (tick `div` 150 + miId * 19) * 0.7
                radio  = min wx wy * 0.45
            in  centro ^+^ V2 (cos angulo * radio) (sin angulo * radio)
          _ ->
            posicionExploracion miId tick (worldSize gs) pR

        destinoBase = if cercaBorde then centro else destinoPatrulla
      
        dirHaciaDestino = normalize (destinoBase ^-^ pR)
        destinoRecto = pR ^+^ (dirHaciaDestino ^* 120)
        
        hayObstaculo = hayObstaculoEnCamino pR destinoRecto obstaculos
        destinoSinObstaculos = if hayObstaculo
                              then let perpDir = perp dirHaciaDestino
                                       giroDir = if miId `mod` 2 == 0 then perpDir else perpDir ^* (-1)
                                   in pR ^+^ (giroDir ^* 120)
                              else destinoRecto
        
        fuerzaRepulsion = foldl (\acc a -> 
          let dist = distanceBetween pR (position a)
              dir = normalize (pR ^-^ position a)
              fuerza = if dist < distanciaMinAliados && dist > 1
                      then 70 * ((distanciaMinAliados - dist) / distanciaMinAliados)
                      else 0
          in acc ^+^ (dir ^* fuerza)
          ) (pure 0) aliados
        
        destinoConRepulsion = destinoSinObstaculos ^+^ (fuerzaRepulsion ^* 0.4)

   
        bodyTargetAngle  = rad2deg (angleToTarget pR destinoConRepulsion)
        newBodyAngle     = smoothRotateTowards currentBodyAngle bodyTargetAngle velocidadRotacion

        barrido          = sin (fromIntegral tick * 0.04) * 40
        cannonTargetAng  = bodyTargetAngle + barrido
        newCannonAngle   = smoothRotateTowards currentCannonAngle cannonTargetAng velocidadRotacion

      in accionRol ++
         [ Rotate (deg2rad newBodyAngle)
         , RotateCannon (deg2rad newCannonAngle)
         , Move destinoConRepulsion
         ]

    -- ================== CON ENEMIGO ==================
    Just zombie ->
      let
        pZ  = position zombie
        idZ = objectId zombie
        dist = distanceBetween pR pZ
        

        dir = normalize (pZ ^-^ pR)
        dirHuida = normalize (pR ^-^ pZ)
        
        -- Cañón apunta al enemigo
        cannonTargetAng = rad2deg (angleToTarget pR pZ)
        newCannonAngle  = smoothRotateTowards currentCannonAngle cannonTargetAng velocidadRotacion
        
        -- Verificar si puede disparar
        canionDir = let t = deg2rad newCannonAngle in V2 (cos t) (sin t)
        estoyApuntando = canionDir `dot` dir > 0.90
        puedeDisparar = tick `mod` cadenciaDisparo == 0
        hayObstaculo = hayObstaculoEnCamino pR pZ obstaculos
        
        -- Movimiento táctico según rol
        vidaBaja = vida < 40
        
        destinoBase
          | cercaBorde = centro
          | vidaBaja && dist < 100 = pR ^+^ dirHuida ^* 100  -- Huir si está bajo de vida y cerca
          | dist < distanciaPreferida - 40 = pR ^+^ dirHuida ^* 60  -- Retroceder si muy cerca
          | dist > distanciaPreferida + 40 = pZ  -- Avanzar si muy lejos
          | otherwise = -- Strafear manteniendo distancia
              let perpDir = perp dir
                  strafeDir = if tick `mod` 200 < 100 then perpDir else perpDir ^* (-1)
              in pR ^+^ (strafeDir ^* 50)
        
        -- Si hay obstáculo enfrente, desviarse
        dirHaciaDestino = normalize (destinoBase ^-^ pR)
        destinoRecto = pR ^+^ (dirHaciaDestino ^* 100)
        hayObstaculoEnfrente = hayObstaculoEnCamino pR destinoRecto obstaculos
        
        destinoSinObstaculos = if hayObstaculoEnfrente
                              then let perpDir = perp dirHaciaDestino
                                       opcion1 = pR ^+^ (perpDir ^* 100)
                                       opcion2 = pR ^+^ (perpDir ^* (-100))
                                       dist1 = distanceBetween opcion1 pZ
                                       dist2 = distanceBetween opcion2 pZ
                                   in if dist1 < dist2 then opcion1 else opcion2
                              else destinoRecto
        
        -- Repulsión con aliados
        fuerzaRepulsion = foldl (\acc a -> 
          let distAliado = distanceBetween pR (position a)
              dirAliado = normalize (pR ^-^ position a)
              fuerza = if distAliado < distanciaMinAliados && distAliado > 1
                      then 60 * ((distanciaMinAliados - distAliado) / distanciaMinAliados)
                      else 0
          in acc ^+^ (dirAliado ^* fuerza)
          ) (pure 0) aliados
        
        destinoFinal = destinoSinObstaculos ^+^ (fuerzaRepulsion ^* 0.35)
        
        -- Cuerpo hacia donde se mueve
        bodyTargetAngle = rad2deg (angleToTarget pR destinoFinal)
        newBodyAngle = smoothRotateTowards currentBodyAngle bodyTargetAngle velocidadRotacion
        
        -- Disparo
        accionDisparo = if estoyApuntando && puedeDisparar && not hayObstaculo then [Shoot] else []
        
      in accionRol ++
         [ Rotate (deg2rad newBodyAngle)
         , RotateCannon (deg2rad newCannonAngle)
         , Move destinoFinal
         ] ++ accionDisparo


-- ZOMBIES: Movimiento en línea recta y constante hacia los enemigos
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
    miId = objectId r
    
    -- Velocidad de rotación rápida para reaccionar rápido
    velocidadRotacion = 3.5 :: Float
    
    -- Distancia mínima entre zombies (AUMENTADA para evitar superposición)
    distanciaMinAliados = 90 :: Float
    aliados = aliadosCercanos r enemigosRadar
  
  in case seleccionarObjetivoPrioritario r enemigosRadar of
    Nothing ->
      let 
          -- MOVIMIENTO CONTINUO EN LÍNEA RECTA - nunca se detiene
          -- Patrulla agresiva: muévete en línea recta hacia un punto, cuando llegas escoge otro
          destinoPatrulla =
            let angulo = fromIntegral (tick `div` 180 + miId * 100) * 0.5  -- Cambia de dirección cada 180 ticks
                radio = min wx wy * 0.4
            in centro ^+^ V2 (cos angulo * radio) (sin angulo * radio)
          
          -- Si está cerca del borde, ir directo al centro
          destinoBase = if cercaBorde then centro else destinoPatrulla
          
          -- Calcular dirección sin importar obstáculos (los atravesará o rodeará en tiempo real)
          dirMovimiento = normalize (destinoBase ^-^ pR)
          
          -- Punto adelante en línea recta
          destinoRecto = pR ^+^ (dirMovimiento ^* 150)
          
          -- Solo cambiar dirección si hay obstáculo JUSTO enfrente
          destinoFinal = if hayObstaculoEnCamino pR destinoRecto obstaculos
                        then 
                          -- Girar 90 grados y seguir adelante
                          let perpDir = perp dirMovimiento
                              -- Alterna entre izquierda y derecha según el ID
                              giroDir = if miId `mod` 2 == 0 then perpDir else perpDir ^* (-1)
                          in pR ^+^ (giroDir ^* 150)
                        else destinoRecto
          
          -- Repulsión más fuerte para evitar superposición
          fuerzaRepulsion = foldl (\acc a -> 
            let dist = distanceBetween pR (position a)
                dir = normalize (pR ^-^ position a)
                fuerza = if dist < distanciaMinAliados && dist > 1
                        then 60 * ((distanciaMinAliados - dist) / distanciaMinAliados)  -- Aumentado de 30 a 60
                        else 0
            in acc ^+^ (dir ^* fuerza)
            ) (pure 0) aliados
          
          destinoConRepulsion = destinoFinal ^+^ (fuerzaRepulsion ^* 0.3)  -- Aumentado de 0.15 a 0.3
          
          targetAngle = rad2deg (angleToTarget pR destinoConRepulsion)
          newAngle = smoothRotateTowards currentAngle targetAngle velocidadRotacion
          
      in [Rotate (deg2rad newAngle), Move destinoConRepulsion]
    
    Just humano ->
      let
        pH = position humano
        idH = objectId humano
        dist = distanceBetween pR pH
        
        -- Dirección directa al humano
        dirHaciaHumano = normalize (pH ^-^ pR)
        
        -- MOVIMIENTO SIEMPRE EN LÍNEA RECTA hacia el humano
        distanciaIdeal = 100  -- Distancia ideal de ataque
        
        -- Calcular destino en línea recta
        destinoDirecto = if dist > distanciaIdeal
                        then pH  -- Ir directo al humano
                        else pR ^+^ (dirHaciaHumano ^* 50)  -- Avanzar un poco para mantener presión
        
        -- Solo desviarse si hay un obstáculo justo enfrente
        hayObstaculoEnfrente = hayObstaculoEnCamino pR destinoDirecto obstaculos
        
        destinoBase = if hayObstaculoEnfrente
                     then 
                       -- Rodear el obstáculo manteniendo la dirección general hacia el humano
                       let perpDir = perp dirHaciaHumano
                           -- Intentar ir por el lado que esté más cerca del humano
                           opcion1 = pR ^+^ (perpDir ^* 100)
                           opcion2 = pR ^+^ (perpDir ^* (-100))
                           dist1 = distanceBetween opcion1 pH
                           dist2 = distanceBetween opcion2 pH
                       in if dist1 < dist2 then opcion1 else opcion2
                     else destinoDirecto
        
        -- Repulsión más fuerte para evitar superposición durante ataque
        fuerzaRepulsion = foldl (\acc a -> 
          let dist' = distanceBetween pR (position a)
              dir' = normalize (pR ^-^ position a)
              fuerza = if dist' < distanciaMinAliados && dist' > 1
                      then 50 * ((distanciaMinAliados - dist') / distanciaMinAliados)  -- Aumentado de 25 a 50
                      else 0
          in acc ^+^ (dir' ^* fuerza)
          ) (pure 0) aliados
        
        destinoFinal = if cercaBorde 
                      then centro  -- Si está en el borde, volver al centro
                      else destinoBase ^+^ (fuerzaRepulsion ^* 0.25)  -- Aumentado de 0.1 a 0.25
        
        -- ROTACIÓN hacia el destino
        bodyTargetAngle = rad2deg (angleToTarget pR destinoFinal)
        newBodyAngle = smoothRotateTowards currentAngle bodyTargetAngle velocidadRotacion
        
        -- DISPARO: Apuntar al humano mientras se mueve
        shootTargetAngle = rad2deg (angleToTarget pR pH)
        anguloDisparo = abs (newBodyAngle - shootTargetAngle)
        apuntaBien = anguloDisparo < 30  -- Más tolerante
        cooldownDisparo = 30
        puedeDisparar = dist < 250 && apuntaBien && 
                       tick `mod` cooldownDisparo == (objectId r `mod` cooldownDisparo)
        accionesAtaque = if puedeDisparar then [Shoot] else []
        
      in [Rotate (deg2rad newBodyAngle), Move destinoFinal] ++ accionesAtaque
