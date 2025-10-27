module Robot
  ( detectedAgent, isRobotAlive, countActiveRobots
  , updateRobotVelocity, updateVelocity, updatePosition, botDecision
  , hasLineOfSight, esEnemigo, robotsCercanos, aliadosCercanos
  ) where

import Entidades
import Fisicas
import Control.Applicative (liftA2)

detectedAgent :: Robot -> Robot -> Bool
detectedAgent r1 r2 = distanceBetween (position r1) (position r2) <= range (extras r1)

isRobotAlive :: Robot -> Bool
isRobotAlive r = energy (extras r) > 0

countActiveRobots :: [Robot] -> Int
countActiveRobots = length . filter isRobotAlive

updateRobotVelocity :: Robot -> Velocity -> Robot
updateRobotVelocity r v = r { velocity = v }

updateVelocity :: Action -> Robot -> Robot
updateVelocity (Action stop dir a _shoot) r
  | stop      = r { velocity = pure 0 }
  | otherwise = r { velocity = velocity r ^+^ (dir ^* a) }

updatePosition :: Robot -> Tiempo -> Robot
updatePosition r dt = 
  let newPos = position r ^+^ (velocity r ^* dt)
  in r { position = newPos }

hasLineOfSight :: Position -> Position -> [Robot] -> Bool
hasLineOfSight p1 p2 obstacles = True

-- NUEVO: Verificar si dos robots son enemigos
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

-- MODIFICADO: Filtrar solo enemigos (diferente tipo)
-- Detecta robots cercanos, tanto enemigos como aliados
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

seleccionarObjetivoPrioritario :: Robot -> [Robot] -> Maybe Robot
seleccionarObjetivoPrioritario self [] = Nothing
seleccionarObjetivoPrioritario self enemigos =
  let pR = position self
      scoreEnemigo e =
        let dist = distanceBetween pR (position e)
            vida = energy (extras e)
            vidaScore = vida * 2.0
            distScore = dist / 2.0
        in vidaScore + distScore
      mejorEnemigo = foldl1 (\e1 e2 -> if scoreEnemigo e1 < scoreEnemigo e2 then e1 else e2) enemigos
  in Just mejorEnemigo

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

botDecision :: Int -> GameState -> Robot -> [Robot] -> [BotAction]
botDecision tick gs self others =
  let enemigos = enemigosEnRadar self others
  in case tipo (extras self) of
       Humano  -> estrategiaHumano tick gs self enemigos
       Zombie  -> estrategiaZombie tick gs self enemigos

-- HUMANOS: Disparan a los zombies
estrategiaHumano :: Int -> GameState -> Robot -> [Robot] -> [BotAction]
estrategiaHumano tick gs r enemigosRadar =
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
    
    aliados = aliadosCercanos r enemigosRadar
    distanciaMinAliados = 100
    distanciaOptima = 150
    
    centroGrupo = if null aliados 
                  then pR
                  else let sumPos = foldl (\acc a -> acc ^+^ position a) pR aliados
                       in (1 / fromIntegral (length aliados + 1)) *^ sumPos
    
    -- LÓGICA 1: SISTEMA DE ROLES TÁCTICOS DINÁMICO
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
      "tank"    -> (rango * 0.60, 40, 5.0)
      "sniper"  -> (rango * 0.85, 60, 3.5)
      "support" -> (rango * 0.70, 50, 4.5)
      _         -> (rango * 0.75, 55, 4.0)
    
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
          
          destinoBase = if cercaBorde then centro
                       else if debeInvestigar 
                            then maybe destinoPatrulla id memLast
                       else destinoPatrulla
          
          destinoFinal = ajustarFormacionDefensiva destinoBase
          
          barrido = sin (fromIntegral tick * 0.04) * 60
          targetAngle = rad2deg (angleToTarget pR destinoFinal)
          newCannonAngle = smoothRotateTowards currentCannonAngle 
                                               (targetAngle + barrido) 
                                               velocidadRotacion
          
          accionesMemoria = [SetAggroCooldown nuevoCooldown]
          
      in accionRol ++ accionesMemoria ++ 
         [RotateCannon (deg2rad newCannonAngle), Move destinoFinal]
    
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
        
        destino =
          if cercaBorde then centro
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
            pR ^+^ dir ^* 70
          else
            let perpDir = perp dir
                strafeDir = if tick `mod` 200 < 100 then perpDir else perpDir ^* (-1)
            in pR ^+^ (strafeDir ^* 40)
        
        destinoFinal = ajustarFormacionDefensiva destino
        
        accionDisparo = if estoyApuntando && puedeDisparar 
                        then [Shoot] 
                        else []
        
      in accionRol ++ accionesMemoria ++ 
         [RotateCannon (deg2rad newCannonAngle), Move destinoFinal] ++ 
         accionDisparo

-- ZOMBIES: Persiguen y atacan a los humanos
estrategiaZombie :: Int -> GameState -> Robot -> [Robot] -> [BotAction]
estrategiaZombie tick gs r enemigosRadar =
  let
    pR = position r
    V2 wx wy = worldSize gs
    margen = 100
    cercaBorde = vx pR < margen || vx pR > wx - margen || 
                 vy pR < margen || vy pR > wy - margen
    centro = V2 (wx / 2) (wy / 2)
    
    currentAngle = angulo r
    vida = energy (extras r)
    
    -- MEMORIA SIMPLE: Solo recordar último objetivo visto
    memLast = memLastSeen (extras r)
    memCooldown = memAggroCooldown (extras r)
    
    aliados = aliadosCercanos r enemigosRadar
    distanciaMinAliados = 80
    
    velocidadRotacion = 6.0
    
  in case seleccionarObjetivoPrioritario r enemigosRadar of
    Nothing ->
      -- SIN ENEMIGOS: Explorar o investigar última posición
      let 
          nuevoCooldown = max 0 (memCooldown - 1)
          
          -- Si tenemos memoria reciente, ir a investigar
          debeInvestigar = nuevoCooldown > 30 && memLast /= Nothing
          
          destinoBase = case memLast of
            Just lastPos | debeInvestigar && distanceBetween pR lastPos > 50 -> 
              lastPos  -- Ir a donde vimos al humano
            _ -> 
              -- Patrullar en círculos
              let angulo = fromIntegral (tick + objectId r * 100) * 0.01
                  radio = min wx wy * 0.35
              in centro ^+^ V2 (cos angulo * radio) (sin angulo * radio)
          
          destinoFinal = if cercaBorde then centro else destinoBase
          
          -- Evitar aliados cercanos
          fuerzaRepulsion = foldl (\acc a -> 
            let dist = distanceBetween pR (position a)
                dir = normalize (pR ^-^ position a)
            in if dist < distanciaMinAliados 
               then acc ^+^ (dir ^* 150)
               else acc
            ) (pure 0) aliados
          
          destinoConRepulsion = destinoFinal ^+^ fuerzaRepulsion
          
          targetAngle = rad2deg (angleToTarget pR destinoConRepulsion)
          newAngle = smoothRotateTowards currentAngle targetAngle velocidadRotacion
          
          -- Limpiar memoria si cooldown llegó a 0
          accionesMemoria = if nuevoCooldown == 0
                           then [SetTarget Nothing, SetAggroCooldown 0]
                           else [SetAggroCooldown nuevoCooldown]
          
      in accionesMemoria ++ [Rotate (deg2rad newAngle), Move destinoConRepulsion]
    
    Just humano ->
      -- CON ENEMIGO: Perseguir agresivamente
      let
        pH = position humano
        idH = objectId humano
        dist = distanceBetween pR pH
        
        -- GUARDAR EN MEMORIA: Objetivo y última posición vista
        accionesMemoria = 
          [ SetTarget (Just idH)
          , UpdateLastSeen pH
          , SetAggroCooldown 120  -- Recordar durante 2 segundos (120 ticks)
          ]
        
        -- Dirección hacia el humano
        dir = normalize (pH ^-^ pR)
        
        -- Evitar aliados mientras persiguen
        fuerzaRepulsion = foldl (\acc a -> 
          let dist' = distanceBetween pR (position a)
              dir' = normalize (pR ^-^ position a)
          in if dist' < distanciaMinAliados 
             then acc ^+^ (dir' ^* 120)
             else acc
          ) (pure 0) aliados
        
        -- Zombies atacan muy de cerca
        distanciaAtaque = 60
        
        destino = if cercaBorde 
                 then centro
                 else if dist > distanciaAtaque 
                      then pH  -- Perseguir directamente
                      else pR ^+^ (dir ^* 25)  -- Mantener presión
        
        destinoFinal = destino ^+^ (fuerzaRepulsion ^* 0.4)
        
        targetAngle = rad2deg (angleToTarget pR destinoFinal)
        newAngle = smoothRotateTowards currentAngle targetAngle velocidadRotacion
        
      in accionesMemoria ++ [Rotate (deg2rad newAngle), Move destinoFinal]