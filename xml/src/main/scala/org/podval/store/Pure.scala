package org.podval.store

// TODO maybe pre-calculate a lazy map from all names to stores?
trait Pure[+T <: Store] extends Stores[T]:
  final override def stores: Seq[T] = storesPure

  protected def storesPure: Seq[T]

object Pure:
  trait With[+T <: Store](override val storesPure: Seq[T]) extends Pure[T]
