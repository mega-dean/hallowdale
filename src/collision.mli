open Types

val between_shapes : shape -> shape -> bool
val between_rects : rect -> rect -> collision option
val with_entity : entity -> rect -> collision option
val between_entities : entity -> entity -> bool
val with_slash' : slash -> rect -> collision option
val with_slash : slash -> sprite -> collision option
