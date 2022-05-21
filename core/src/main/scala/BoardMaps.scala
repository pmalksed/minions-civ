package minionsgame.core

object BoardMaps {
  private def make(xSize: Int, ySize: Int, s: String): (() => BoardState) = { () =>
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
    BoardState.create(plane)
  }

  val empty = make(10,10, """
 . . . . . . . . . .
  . . . . . . . . . .
   . . . . . . . . . .
    . . . . . . . . . .
     . . . . . . . . . .
      . . . . . . . . . .
       . . . . . . . . . .
        . . . . . . . . . .
         . . . . . . . . . .
          . . . . . . . . . .
""")

  val testMap = make(10,10, """
 . . . . . . . . g w
  . . . . . g . . . .
   g . . . . . . . . .
    . . . . . . g w . g
     . . . w . . w w . .
      . . w w . . w . . .
       g . w g . . . . . .
        . . . . . . . . . g
         . . . . g . . . . .
          w g . . . . . . . .
""")


val civMapSize2 = make(13,13, """
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



  val civMapSize3 = make(15,15, """
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

val civMapSize4 = make(19,19, """
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

  val apocalypse = make(10,10, """
 . . g . . . . . g z
  w . . . . . . . . .
   w . . . g . . . . .
    w . . . . . . . . .
     . . w w . . g w . g
      g . w g . . w w . .
       . . . . . . . . . w
        . . . . . g . . . w
         . . . . . . . . . w
          z g . . . . . g . .
""")

  val blackenedShores = make(10,10, """
 . . g . . . g . . .
  w . . . . . . . . .
   w . . . . . . . . .
    w . . . . g . . . g
     g w . . . . g . . .
      w . . w w . . . . .
       w g . . w . . . . .
        w . w . . . . . . g
         . . . g . w . . . .
          . . w w w g w w w .
""")

  val chaosDiamond = make(10,10, """
 . . . w . . . . g .
  . g . . g . . t . w
   w . . . . . . . w w
    w . . w . . . w . .
     . . . . g . . . g .
      . g . . . g . . . .
       . . w . . . w . . w
        w w . . . . . . . w
         w . t . . g . . g .
          . g . . . . w . . .
""")

  val eternalBattlefield = make(10,10, """
 w w . g . . . . . g
  w . . . . . g . . .
   . . . . . . . . . .
    . . . . . . . . g .
     . g w . . . . . . .
      w w g . . . . . . .
       w . . . . . . . . g
        . . . . g w . . . .
         . g . . w g . . . w
          . . . w w . . . w w
""")

  val forbiddenIsle = make(10,10, """
 w w w w w w w w w w
  w w . . g . w g . w
   w g . . . . . . . w
    w . . . . . . . . w
     w . . g w . . . g w
      w g . . . w g . . w
       w . . . . . . . . w
        w . . . . . . . g w
         w . g w . g . . w w
          w w w w w w w w w w
""")

  val megaPuddles = make(10,10, """
 w . . w . . w . . w
  . g . . w . . g . .
   . . . . . w . . w .
    w . . g . . w . . g
     . w . . g . . w . .
      . . w . . g . . w .
       g . . w . . g . . w
        . w . . w . . . . .
         . . g . . w . . g .
          w . . w . . w . . w
""")

  val midnightLake = make(10,10, """
 . g . w w . . . g .
  . . . . . . . . . g
   . . . . . . . . . .
    g . . g . w w . . .
     w . . . w w w . . .
      . . . . w w . . . w
       . g . . . . g . . w
        . . . . . . . . . .
         . . . g . . . . . g
          . . . . . w g . . .
""")

  val riverStyx = make(10,10, """
 . . . . w w . . g .
  . g . . w . . . . .
   . . . g . . . . . .
    . . . . . . . w . .
     . . w w . . g w g .
      . g w g . . w w . .
       . . w . . . . . . .
        . . . . . . g . . .
         . . . . . w . . g .
          . g . . w w . . . .

    """)

  val sorcerersLair = make(10,10, """
 s w . . . g . . w w
  w . . . . w . g . w
   g . . . w . . . g .
    . . . . . . . . . .
     w . g . s . . . w g
      . . w . . s . w . .
       . . . . . . . . . .
        g . . . w g . . . .
         . w . . . . . . . w
          w . g . . w . g w s
""")

  val treacherousPathways = make(10,10, """
 . g . . . . . g . .
  . . . . . . . a . .
   . . f . g . . W a g
    . . . . . . . . . .
     g . . . . . . . . .
      w e w w F . . g . .
       . . . . w . . . . .
        . . . . w . . f . .
         g A . . e . . . . g
          E g . . w g . . . .
""")

  val treacherousPathwaysMist = make(10,10, """
 . g . . . . . g . .
  . m . . . . . m . .
   . . . . g . . w m g
    . . . . . . . . . .
     g . . . . . . . . .
      w m w w m . . g . .
       . . . . w . . . . .
        . . . . w . . . . .
         g w . . m . . . m g
          . g . . w g . . . .
""")

  val advancedMaps = Map()

  val basicMaps = Map(
    "Civ Map Size 4" -> civMapSize4
  )

  val groundImage = Map(
    "Civ Map Size 4" -> "img_terrain_grass0",
  )

  val waterImage = Map(
    "Civ Map Size 4" -> "img_terrain_water1",
  )
}
