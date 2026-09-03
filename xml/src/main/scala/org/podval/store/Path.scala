package org.podval.store

type Path = Seq[Store]

object Path:
  def last[T](path: Path): T = path.last.asInstanceOf[T]
