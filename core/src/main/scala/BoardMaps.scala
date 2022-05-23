package minionsgame.core

object BoardMaps {
  private def make(xSize: Int, ySize: Int, numPlayers: Int, s: String): (() => BoardState) = { () =>
    val map: Array[Terrain] =
      s.toArray.flatMap {
        case '.' => Some(Ground)
        case 'w' => Some(Water(false))
        case 'g' => Some(Graveyard)
        case 's' => Some(SorceryNode)
        case 't' => Some(Teleporter)
        case 'z' => Some(Spawner(Units.zombie.name))
        case 'm' => Some(Mist)
        case 'f' => Some(Firestorm(false))
        case 'a' => Some(Whirlwind(false))
        case 'e' => Some(Earthquake(false))

        case 'W' => Some(Water(true))
        case 'F' => Some(Firestorm(true))
        case 'A' => Some(Whirlwind(true))
        case 'E' => Some(Earthquake(true))

        case _ => None
      }
    val plane = new Plane(xSize,ySize,HexTopology,map)
    BoardState.create(plane, numPlayers)
  }


val civMapSize2 = make(13,13, 2, """
 w w w w w w . . . . . . . 
  w w w w w . . . . . . . . 
   w w w w . . . . . . . . . 
    w w w . . . . . . . . . .
     w w . . . . . . . . . . .
      w . . . . . . . . . . . .
       . . . . . . . . . . . . .
        . . . . . . . . . . . . w
         . . . . . . . . . . . w w
          . . . . . . . . . . w w w
           . . . . . . . . . w w w w
            . . . . . . . . w w w w w
             . . . . . . . w w w w w w
""")



  val civMapSize3 = make(15,15, 3, """
 w w w w w w w . . . . . . . . 
  w w w w w w . . . . . . . . . 
   w w w w w . . . . . . . . . . 
    w w w w . . . . . . . . . . .
     w w w . . . . . . . . . . . .
      w w . . . . . . . . . . . . .
       w . . . . . . . . . . . . . .
        . . . . . . . . . . . . . . .
         . . . . . . . . . . . . . . w
          . . . . . . . . . . . . . w w
           . . . . . . . . . . . . w w w
            . . . . . . . . . . . w w w w
             . . . . . . . . . . w w w w w
              . . . . . . . . . w w w w w w
               . . . . . . . . w w w w w w w
""")

val civMapSize4 = make(19,19, 4, """
 w w w w w w w w w . . . . . . . . . .  
  w w w w w w w w . . . . . . . . . . . 
   w w w w w w w . . . . . . . . . . . .  
    w w w w w w . . . . . . . . . . . . . 
     w w w w w . . . . . . . . . . . . . . 
      w w w w . . . . . . . . . . . . . . . 
       w w w . . . . . . . . . . . . . . . . 
        w w . . . . . . . . . . . . . . . . .
         w . . . . . . . . . . . . . . . . . .
          . . . . . . . . . . . . . . . . . . .
           . . . . . . . . . . . . . . . . . . w
            . . . . . . . . . . . . . . . . . w w
             . . . . . . . . . . . . . . . . w w w
              . . . . . . . . . . . . . . . w w w w
               . . . . . . . . . . . . . . w w w w w
                . . . . . . . . . . . . . w w w w w w
                 . . . . . . . . . . . . w w w w w w w
                  . . . . . . . . . . . w w w w w w w w
                   . . . . . . . . . . w w w w w w w w w
""")

val civMapSize5 = make(21,21, 5, """
 w w w w w w w w w w . . . . . . . . . . .  
  w w w w w w w w w . . . . . . . . . . . . 
   w w w w w w w w . . . . . . . . . . . . .  
    w w w w w w w . . . . . . . . . . . . . . 
     w w w w w w . . . . . . . . . . . . . . . 
      w w w w w . . . . . . . . . . . . . . . . 
       w w w w . . . . . . . . . . . . . . . . . 
        w w w . . . . . . . . . . . . . . . . . .
         w w . . . . . . . . . . . . . . . . . . .
          w . . . . . . . . . . . . . . . . . . . .
           . . . . . . . . . . . . . . . . . . . . .
            . . . . . . . . . . . . . . . . . . . . w
             . . . . . . . . . . . . . . . . . . . w w
              . . . . . . . . . . . . . . . . . . w w w
               . . . . . . . . . . . . . . . . . w w w w
                . . . . . . . . . . . . . . . . w w w w w
                 . . . . . . . . . . . . . . . w w w w w w
                  . . . . . . . . . . . . . . w w w w w w w
                   . . . . . . . . . . . . . w w w w w w w w
                    . . . . . . . . . . . . w w w w w w w w w
                     . . . . . . . . . . . w w w w w w w w w w
""")    


val civMapSize6 = make(23,23, 6, """
 w w w w w w w w w w w . . . . . . . . . . . .  
  w w w w w w w w w w . . . . . . . . . . . . . 
   w w w w w w w w w . . . . . . . . . . . . . .  
    w w w w w w w w . . . . . . . . . . . . . . . 
     w w w w w w w . . . . . . . . . . . . . . . . 
      w w w w w w . . . . . . . . . . . . . . . . . 
       w w w w w . . . . . . . . . . . . . . . . . . 
        w w w w . . . . . . . . . . . . . . . . . . .
         w w w . . . . . . . . . . . . . . . . . . . .
          w w . . . . . . . . . . . . . . . . . . . . .
           w . . . . . . . . . . . . . . . . . . . . . .
            . . . . . . . . . . . . . . . . . . . . . . .
             . . . . . . . . . . . . . . . . . . . . . . w
              . . . . . . . . . . . . . . . . . . . . . w w
               . . . . . . . . . . . . . . . . . . . . w w w
                . . . . . . . . . . . . . . . . . . . w w w w
                 . . . . . . . . . . . . . . . . . . w w w w w
                  . . . . . . . . . . . . . . . . . w w w w w w
                   . . . . . . . . . . . . . . . . w w w w w w w
                    . . . . . . . . . . . . . . . w w w w w w w w
                     . . . . . . . . . . . . . . w w w w w w w w w
                      . . . . . . . . . . . . . w w w w w w w w w w
                       . . . . . . . . . . . . w w w w w w w w w w w

""")  

// val civMapSize6 = make(25,25, """
//  w w w w w w w w w w w w . . . . . . . . . . . . .  
//   w w w w w w w w w w w . . . . . . . . . . . . . . 
//    w w w w w w w w w w . . . . . . . . . . . . . . .  
//     w w w w w w w w w . . . . . . . . . . . . . . . . 
//      w w w w w w w w . . . . . . . . . . . . . . . . . 
//       w w w w w w w . . . . . . . . . . . . . . . . . . 
//        w w w w w w . . . . . . . . . . . . . . . . . . . 
//         w w w w w . . . . . . . . . . . . . . . . . . . .
//          w w w w . . . . . . . . . . . . . . . . . . . . .
//           w w w . . . . . . . . . . . . . . . . . . . . . .
//            w w . . . . . . . . . . . . . . . . . . . . . . .
//             w . . . . . . . . . . . . . . . . . . . . . . . .
//              . . . . . . . . . . . . . . . . . . . . . . . . .
//               . . . . . . . . . . . . . . . . . . . . . . . . w
//                . . . . . . . . . . . . . . . . . . . . . . . w w
//                 . . . . . . . . . . . . . . . . . . . . . . w w w
//                  . . . . . . . . . . . . . . . . . . . . . w w w w
//                   . . . . . . . . . . . . . . . . . . . . w w w w w
//                    . . . . . . . . . . . . . . . . . . . w w w w w w
//                     . . . . . . . . . . . . . . . . . . w w w w w w w
//                      . . . . . . . . . . . . . . . . . w w w w w w w w
//                       . . . . . . . . . . . . . . . . w w w w w w w w w
//                        . . . . . . . . . . . . . . . w w w w w w w w w w
//                         . . . . . . . . . . . . . . w w w w w w w w w w w
//                          . . . . . . . . . . . . . w w w w w w w w w w w w

// """)  



  val advancedMaps = Map()

  val basicMaps = Map(
    "Civ Map Size 2" -> civMapSize2,
    "Civ Map Size 3" -> civMapSize3,
    "Civ Map Size 4" -> civMapSize4,
    "Civ Map Size 5" -> civMapSize5,
    "Civ Map Size 6" -> civMapSize6,
  )

  val groundImage = Map(
    "Civ Map Size 2" -> "img_terrain_grass0",
    "Civ Map Size 3" -> "img_terrain_grass0",
    "Civ Map Size 4" -> "img_terrain_grass0",
    "Civ Map Size 5" -> "img_terrain_grass0",
    "Civ Map Size 6" -> "img_terrain_grass0",
  )

  val waterImage = Map(
    "Civ Map Size 2" -> "img_terrain_water1",
    "Civ Map Size 3" -> "img_terrain_water1",
    "Civ Map Size 4" -> "img_terrain_water1",
    "Civ Map Size 5" -> "img_terrain_water1",
    "Civ Map Size 6" -> "img_terrain_water1",
  )
}
