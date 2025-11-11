module Robot
  ( isRobotAlive
  , updateRobotVelocity, updatePosition, botDecision
  , esEnemigo, robotsCercanos, aliadosCercanos
  , separarRobots
  , actualizarHistorialPosicion, actualizarContadorEstancamiento, marcarDestinoFallido
  ) where

import Entidades
import Fisicas
import Data.List (minimumBy)
import Data.Ord  (comparing)

-- ================= Utilidad básica =================
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

-- Rotación con inercia
smoothRotateTowards :: Angle -> Angle -> Float -> Angle
smoothRotateTowards current target speed =
  let diff = normalizeAngle (target - current)
      distToTarget = abs diff
      easedSpeed = if distToTarget < 15 then speed * (distToTarget / 15) else speed
      step = clamp (-easedSpeed) easedSpeed diff
  in current + step
  where
    normalizeAngle a
      | a > 180   = a - 360
      | a < -180  = a + 360
      | otherwise = a
    clamp lo hi v = max lo (min hi v)

-- ================= Memoria / heurísticas =================
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

-- ================= Percepción =================
robotsCercanos :: Robot -> [Robot] -> [Robot]
robotsCercanos self todos =
  let rango = range (extras self)
      pR = position self
  in filter (\e -> objectId e /= objectId self &&
                   distanceBetween pR (position e) <= rango) todos

enemigosEnRadar :: Robot -> [Robot] -> [Robot]
enemigosEnRadar self todos =
  let rango = range (extras self)
      pR = position self
  in filter (\e -> esEnemigo self e && distanceBetween pR (position e) <= rango) todos

-- Radar con multiplicador (para caza total)
enemigosEnRango :: Float -> Robot -> [Robot] -> [Robot]
enemigosEnRango mult self todos =
  let rango = range (extras self) * mult
      pR = position self
  in filter (\e -> esEnemigo self e && distanceBetween pR (position e) <= rango) todos

aliadosCercanos :: Robot -> [Robot] -> [Robot]
aliadosCercanos self todos =
  let rango = range (extras self)
      pR = position self
  in filter (\e -> not (esEnemigo self e) &&
                   objectId e /= objectId self &&
                   distanceBetween pR (position e) <= rango) todos

-- Aliados en un radio fijo (para romper aglomeraciones)
aliadosEnRadio :: Distance -> Robot -> [Robot] -> [Robot]
aliadosEnRadio radio self todos =
  let pR = position self
  in filter (\a -> objectId a /= objectId self
                && not (esEnemigo self a)
                && distanceBetween pR (position a) < radio) todos

humanoMasCercanoGlobal :: Robot -> [Robot] -> Maybe Robot
humanoMasCercanoGlobal self todos =
  let pR = position self
      humanos = filter (\x -> tipo (extras x) == Humano) todos
  in if null humanos then Nothing else Just (minimumBy (comparing (distanceBetween pR . position)) humanos)

-- ================= Navegación / obstáculos =================
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
      _  ->
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
                  hayObs = hayObstaculoEnCamino desde opt obstaculos
                  penal = if hayObs then 500 else 0
              in distancia + penal
            opciones = [(opcion1, evaluarOpcion opcion1),
                        (opcion2, evaluarOpcion opcion2),
                        (opcion3, evaluarOpcion opcion3),
                        (opcion4, evaluarOpcion opcion4)]
            mejorOpcion = fst $ foldl1 (\(p1,c1) (p2,c2) -> if c1 < c2 then (p1,c1) else (p2,c2)) opciones
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
    []     -> Nothing
    (obs:_) ->
      let posObs = position obs
          dirObsAEnemigo = normalize (posEnemigo ^-^ posObs)
          posTactica = posObs ^-^ (dirObsAEnemigo ^* 150)
      in Just posTactica

-- === Objetivo: SIEMPRE el más cercano ===
seleccionarObjetivoPrioritario :: Robot -> [Robot] -> Maybe Robot
seleccionarObjetivoPrioritario _   [] = Nothing
seleccionarObjetivoPrioritario self es =
  let pR = position self
  in Just $ minimumBy (comparing (distanceBetween pR . position)) es

posicionExploracion :: Int -> Int -> Size -> Position -> Position
posicionExploracion robotId tick (V2 wx wy) _ =
  let numSectoresX = 4
      numSectoresY = 3
      totalSectores = numSectoresX * numSectoresY
      cicloExploracion = (tick `div` 180) `mod` totalSectores
      sectorBase = (robotId * 7 + cicloExploracion) `mod` totalSectores
      sectorX = sectorBase `mod` numSectoresX
      sectorY = sectorBase `div` numSectoresX
      anchoSector = wx / fromIntegral numSectoresX
      altoSector  = wy / fromIntegral numSectoresY
      margen = 80
      seed = robotId * 1000 + tick `div` 180
      hash n = let a = n * 15485863
                   b = (a `mod` 2038074743)
               in fromIntegral b / 2038074743.0
      offsetX = (hash seed - 0.5) * (anchoSector * 0.6)
      offsetY = (hash (seed + 1) - 0.5) * (altoSector * 0.6)
      baseX = fromIntegral sectorX * anchoSector + anchoSector / 2
      baseY = fromIntegral sectorY * altoSector  + altoSector  / 2
      targetX = clampVal margen (wx - margen) (baseX + offsetX)
      targetY = clampVal margen (wy - margen) (baseY + offsetY)
  in V2 targetX targetY
  where clampVal lo hi v = max lo (min hi v)

-- ================= Decisión principal =================
botDecision :: Int -> GameState -> [Obstaculo] -> Robot -> [Robot] -> [BotAction]
botDecision tick gs obstaculos self others =
  case tipo (extras self) of
    Humano -> estrategiaHumano tick gs obstaculos self others
    Zombie -> estrategiaZombie tick gs obstaculos self others

-- ================= HUMANO =================
estrategiaHumano :: Int -> GameState -> [Obstaculo] -> Robot -> [Robot] -> [BotAction]
estrategiaHumano tick gs obstaculos r todos =
  let pR = position r
      V2 wx wy = worldSize gs
      margen = 100
      cercaBorde = vx pR < margen || vx pR > wx - margen || vy pR < margen || vy pR > wy - margen
      centro = V2 (wx / 2) (wy / 2)

      currentBodyAngle   = angulo r
      currentCannonAngle = anguloCanon r

      vida  = energy (extras r)
      rango = range  (extras r)

      miId = objectId r
      variacionIndividual = fromIntegral (miId `mod` 60)
      distanciaMinAliados = 100 + variacionIndividual

      aliados = aliadosCercanos r todos
      enemigosRadar = enemigosEnRadar r todos

      distanciaPreferida = rango * 0.75
      cadenciaDisparo    = 50
      velocidadRotacion  = 2.0
  in case seleccionarObjetivoPrioritario r enemigosRadar of
    Nothing ->
      let destinoPatrulla   = posicionExploracion miId tick (worldSize gs) pR
          destinoBase0      = if cercaBorde then centro else destinoPatrulla
          dirHaciaDestino   = normalize (destinoBase0 ^-^ pR)
          destinoRecto      = pR ^+^ (dirHaciaDestino ^* 120)
          hayObs            = hayObstaculoEnCamino pR destinoRecto obstaculos
          destinoSinObs     = if hayObs
                                then let perpDir = perp dirHaciaDestino
                                         giroDir = if miId `mod` 2 == 0 then perpDir else perpDir ^* (-1)
                                     in pR ^+^ (giroDir ^* 120)
                                else destinoRecto
          fuerzaRepulsion   = foldl (\acc a ->
                                      let dist = distanceBetween pR (position a)
                                          dir  = normalize (pR ^-^ position a)
                                          fuerza = if dist < distanciaMinAliados && dist > 1
                                                   then 70 * ((distanciaMinAliados - dist) / distanciaMinAliados)
                                                   else 0
                                      in acc ^+^ (dir ^* fuerza)
                                    ) (pure 0) aliados
          destinoFinal      = navegarConAntiAtascamiento tick gs obstaculos r (destinoSinObs ^+^ (fuerzaRepulsion ^* 0.4))
          bodyTargetAngle   = rad2deg (angleToTarget pR destinoFinal)
          newBodyAngle      = smoothRotateTowards currentBodyAngle bodyTargetAngle velocidadRotacion
          barrido           = sin (fromIntegral tick * 0.04) * 40
          cannonTargetAng   = bodyTargetAngle + barrido
          newCannonAngle    = smoothRotateTowards currentCannonAngle cannonTargetAng velocidadRotacion
      in [ Rotate (deg2rad newBodyAngle)
         , RotateCannon (deg2rad newCannonAngle)
         , Move destinoFinal
         , SetTarget Nothing
         ]
    Just zombie ->
      let pZ  = position zombie
          dist = distanceBetween pR pZ
          dir = normalize (pZ ^-^ pR)
          dirHuida = normalize (pR ^-^ pZ)

          cannonTargetAng = rad2deg (angleToTarget pR pZ)
          newCannonAngle  = smoothRotateTowards currentCannonAngle cannonTargetAng velocidadRotacion
          canionDir       = let t = deg2rad newCannonAngle in V2 (cos t) (sin t)
          estoyApuntando  = canionDir `dot` dir > 0.90
          puedeDisparar   = tick `mod` cadenciaDisparo == 0
          hayObsDirecto   = hayObstaculoEnCamino pR pZ obstaculos

          vidaBaja        = vida < 40
          destinoBase
            | cercaBorde                       = centro
            | vidaBaja && dist < 100          = pR ^+^ dirHuida ^* 100
            | dist < distanciaPreferida - 40  = pR ^+^ dirHuida ^* 60
            | dist > distanciaPreferida + 40  = pZ
            | otherwise =
                let perpDir = perp dir
                    strafeDir = if tick `mod` 200 < 100 then perpDir else perpDir ^* (-1)
                in pR ^+^ (strafeDir ^* 50)

          dirHaciaDestino = normalize (destinoBase ^-^ pR)
          destinoRecto    = pR ^+^ (dirHaciaDestino ^* 100)
          hayObstaculoFrente = hayObstaculoEnCamino pR destinoRecto obstaculos
          destinoSinObs   = if hayObstaculoFrente
                            then let perpDir = perp dirHaciaDestino
                                     o1 = pR ^+^ (perpDir ^* 100)
                                     o2 = pR ^+^ (perpDir ^* (-100))
                                 in if distanceBetween o1 pZ < distanceBetween o2 pZ then o1 else o2
                            else destinoRecto

          fuerzaRepulsion = foldl (\acc a ->
                                    let distA = distanceBetween pR (position a)
                                        dirA  = normalize (pR ^-^ position a)
                                        fuerza = if distA < distanciaMinAliados && distA > 1
                                                 then 60 * ((distanciaMinAliados - distA) / distanciaMinAliados)
                                                 else 0
                                    in acc ^+^ (dirA ^* fuerza)
                                  ) (pure 0) aliados
          destinoFinal    = destinoSinObs ^+^ (fuerzaRepulsion ^* 0.35)
          bodyTargetAngle = rad2deg (angleToTarget pR destinoFinal)
          newBodyAngle    = smoothRotateTowards currentBodyAngle bodyTargetAngle velocidadRotacion

          accionDisparo   = if estoyApuntando && puedeDisparar && not hayObsDirecto then [Shoot] else []
      in [ Rotate (deg2rad newBodyAngle)
         , RotateCannon (deg2rad newCannonAngle)
         , Move destinoFinal
         ] ++ accionDisparo
           ++ [ SetTarget (Just (objectId zombie))
              , UpdateLastSeen (position zombie)
              ]

-- ================= ZOMBIE (con modo caza total) =================
estrategiaZombie :: Int -> GameState -> [Obstaculo] -> Robot -> [Robot] -> [BotAction]
estrategiaZombie tick gs obstaculos r todos =
  let pR = position r
      V2 wx wy = worldSize gs
      margen = 100
      cercaBorde = vx pR < margen || vx pR > wx - margen ||
                   vy pR < margen || vy pR > wy - margen
      centro = V2 (wx / 2) (wy / 2)
      currentAngle = angulo r
      miId = objectId r
      velocidadRotacion = 3.5 :: Float

      humanosVivos = length (filter (\x -> tipo (extras x) == Humano) (r:todos))
      cazaTotal    = humanosVivos <= 1

      -- separación más fuerte si están en caza total
      distanciaMinAliados = if cazaTotal then 160 else 140

      aliados       = aliadosCercanos r todos
      enemigosRadar = if cazaTotal then enemigosEnRango 1.6 r todos else enemigosEnRadar r todos
  in case seleccionarObjetivoPrioritario r enemigosRadar of
    -- Sin humano a la vista
    Nothing ->
      let -- si queda 1 humano, perseguir su posición global, si sabemos de él
          destinoBaseCaza =
            case (cazaTotal, humanoMasCercanoGlobal r todos) of
              (True, Just h) ->
                let pH = position h
                    dirH = normalize (pH ^-^ pR)
                in pR ^+^ (dirH ^* 220)      -- paso largo hacia el humano
              _ -> posicionExploracion miId tick (worldSize gs) pR

          -- rompe-aglomeraciones si hay muchos juntos
          cercanos = aliadosEnRadio distanciaMinAliados r todos
          destinoSplit  =
            if length cercanos >= 3
            then let centroAli = promedioPosiciones (map position cercanos)
                     dirBreak  = normalize (pR ^-^ centroAli)
                 in pR ^+^ (dirBreak ^* 200)
            else destinoBaseCaza

          -- evitar bordes y obstáculos
          destinoBorde  = if cercaBorde then centro else destinoSplit
          dirMov        = normalize (destinoBorde ^-^ pR)
          destinoRecto  = pR ^+^ (dirMov ^* 170)
          destinoSinObs = if hayObstaculoEnCamino pR destinoRecto obstaculos
                          then let perpDir = perp dirMov
                                   giroDir = if miId `mod` 2 == 0 then perpDir else perpDir ^* (-1)
                               in pR ^+^ (giroDir ^* 170)
                          else destinoRecto

          -- repulsión con aliados
          fuerzaRepulsion = foldl (\acc a ->
                                    let dist = distanceBetween pR (position a)
                                        dir  = normalize (pR ^-^ position a)
                                        fuerza = if dist < distanciaMinAliados && dist > 1
                                                 then 80 * ((distanciaMinAliados - dist) / distanciaMinAliados)
                                                 else 0
                                    in acc ^+^ (dir ^* fuerza)
                                  ) (pure 0) aliados

          destinoConRepulsion = destinoSinObs ^+^ (fuerzaRepulsion ^* 0.35)
          destinoFinal        = navegarConAntiAtascamiento tick gs obstaculos r destinoConRepulsion

          targetAngle = rad2deg (angleToTarget pR destinoFinal)
          newAngle    = smoothRotateTowards currentAngle targetAngle velocidadRotacion

          sprint = if cazaTotal then 60 else 0  -- acelera en caza total
      in [ Rotate (deg2rad newAngle)
         , Move destinoFinal
         ] ++ (if sprint > 0 then [Accelerate (fromIntegral sprint)] else [])
           ++ [ SetTarget Nothing ]

    -- Con humano a la vista
    Just humano ->
      let pH = position humano
          dist = distanceBetween pR pH
          dirHaciaHumano = normalize (pH ^-^ pR)
          distanciaIdeal = 100
          destinoDirecto = if dist > distanciaIdeal then pH else pR ^+^ (dirHaciaHumano ^* 50)
          hayObstaculoEnfrente = hayObstaculoEnCamino pR destinoDirecto obstaculos
          destinoBase = if hayObstaculoEnfrente
                        then let perpDir = perp dirHaciaHumano
                                 o1 = pR ^+^ (perpDir ^* 120)
                                 o2 = pR ^+^ (perpDir ^* (-120))
                             in if distanceBetween o1 pH < distanceBetween o2 pH then o1 else o2
                        else destinoDirecto
          fuerzaRepulsion = foldl (\acc a ->
                                    let dist' = distanceBetween pR (position a)
                                        dir'  = normalize (pR ^-^ position a)
                                        fuerza = if dist' < distanciaMinAliados && dist' > 1
                                                 then 55 * ((distanciaMinAliados - dist') / distanciaMinAliados)
                                                 else 0
                                    in acc ^+^ (dir' ^* fuerza)
                                  ) (pure 0) aliados
          destinoFinal = destinoBase ^+^ (fuerzaRepulsion ^* 0.30)
          bodyTargetAngle = rad2deg (angleToTarget pR destinoFinal)
          newBodyAngle = smoothRotateTowards currentAngle bodyTargetAngle velocidadRotacion

          cooldownDisparo = if cazaTotal then 22 else 30
          shootTargetAngle = rad2deg (angleToTarget pR pH)
          anguloDisparo = abs (newBodyAngle - shootTargetAngle)
          apuntaBien = anguloDisparo < 30
          puedeDisparar = dist < 260 && apuntaBien &&
                          tick `mod` cooldownDisparo == (objectId r `mod` cooldownDisparo)
          accionesAtaque = (if puedeDisparar then [Shoot] else [])
          sprint = if cazaTotal && dist > 160 then 60 else 0
      in [ Rotate (deg2rad newBodyAngle)
         , Move destinoFinal
         ] ++ accionesAtaque
           ++ (if sprint > 0 then [Accelerate (fromIntegral sprint)] else [])
           ++ [ SetTarget (Just (objectId humano))
              , UpdateLastSeen (position humano)
              ]

-- ===== anti-atascamiento =====
detectarEstancamiento :: Robot -> Position -> Bool
detectarEstancamiento r destino =
  let ex = extras r
      posActual = position r
      historial = memPositionHistory ex
      fallidos = memFailedDestinations ex
      contador = memStuckCounter ex
      enMismaArea = case historial of
        (p1:p2:p3:_) ->
          let d1 = distanceBetween posActual p1
              d2 = distanceBetween posActual p2
              d3 = distanceBetween posActual p3
          in d1 < 15 && d2 < 15 && d3 < 15
        _ -> False
      destinoFallido = estaEnHistorialFallido destino fallidos 50
      contadorAlto = contador > 120
  in enMismaArea || destinoFallido || contadorAlto

generarRutaEscape :: Position -> [Obstaculo] -> Int -> Size -> Position
generarRutaEscape posActual obstaculos tick (V2 wx wy) =
  let angulos = [0,45,90,135,180,225,270,315]
      distanciaEscape = 200
      candidatos = [ posActual ^+^ V2 (cos (deg2rad a) * distanciaEscape)
                                  (sin (deg2rad a) * distanciaEscape)
                   | a <- angulos ]
      candidatosLibres = filter (\c ->
        not (hayObstaculoEnCamino posActual c obstaculos) &&
        vx c > 100 && vx c < wx - 100 && vy c > 100 && vy c < wy - 100) candidatos
      mejorCandidato = case candidatosLibres of
        [] ->
          let perpAngulo = fromIntegral ((tick * 37) `mod` 360)
          in posActual ^+^ V2 (cos (deg2rad perpAngulo) * 150)
                               (sin (deg2rad perpAngulo) * 150)
        cs ->
          let obsCercanos = filter (\o -> distanceBetween (position o) posActual < 300) obstaculos
              puntuar c = minimum [ distanceBetween c (position o) | o <- obsCercanos ]
          in if null obsCercanos then head cs
             else foldl1 (\c1 c2 -> if puntuar c1 > puntuar c2 then c1 else c2) cs
  in mejorCandidato

actualizarHistorialPosicion :: Robot -> Robot
actualizarHistorialPosicion r =
  let ex = extras r
      posActual = position r
      historial = memPositionHistory ex
      nuevoHistorial = take 10 (posActual : historial)
  in r { extras = ex { memPositionHistory = nuevoHistorial } }

actualizarContadorEstancamiento :: Robot -> Position -> Robot
actualizarContadorEstancamiento r _ =
  let ex = extras r
      posActual = position r
      ultimaPos = memLastPosition ex
      contador = memStuckCounter ex
      seMovio = case ultimaPos of
        Nothing -> True
        Just p  -> distanceBetween posActual p > 5
      nuevoContador = if seMovio then 0 else contador + 1
  in r { extras = ex { memStuckCounter = nuevoContador, memLastPosition = Just posActual } }

marcarDestinoFallido :: Robot -> Position -> Robot
marcarDestinoFallido r destino =
  let ex = extras r
      fallidos = memFailedDestinations ex
      nuevosFallidos = take 20 (destino : fallidos)
  in r { extras = ex { memFailedDestinations = nuevosFallidos } }

navegarConAntiAtascamiento :: Int -> GameState -> [Obstaculo] -> Robot -> Position -> Position
navegarConAntiAtascamiento tick gs obstaculos r destinoOriginal =
  let posActual = position r
      estaAtascado = detectarEstancamiento r destinoOriginal
      V2 wx wy = worldSize gs
      destinoFinal = if estaAtascado
        then generarRutaEscape posActual obstaculos tick (V2 wx wy)
        else if hayObstaculoEnCamino posActual destinoOriginal obstaculos
             then encontrarRutaRodeando posActual destinoOriginal obstaculos
             else destinoOriginal
  in destinoFinal

cambiarObjetivoSiAtascado :: Robot -> [Robot] -> Maybe Robot
cambiarObjetivoSiAtascado self enemigos =
  let contador = memStuckCounter (extras self)
      objetivoActual = memTarget (extras self)
  in if contador > 180
     then
       let enemigosAlternativos = case objetivoActual of
             Just tid -> filter (\e -> objectId e /= tid) enemigos
             Nothing  -> enemigos
       in seleccionarObjetivoPrioritario self enemigosAlternativos
     else seleccionarObjetivoPrioritario self enemigos
