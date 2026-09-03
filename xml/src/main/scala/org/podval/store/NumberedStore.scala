package org.podval.store

import org.podval.metadata.Names

trait NumberedStore extends Store, org.podval.metadata.Numbered[NumberedStore]:
  def oneOf: NumberedStores[NumberedStore]

  final override def names: Names = oneOf.number2names(number)