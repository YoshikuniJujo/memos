memo
====

	data Free t a
		= Pure a
		| Join (t (Free t a))

	join' :: Functor t => Free t (Free t a) -> Free t a
	join' (Pure m) = m
	join' (Join tm) = Join $ join' <$> tm

	Free [] Integer
		=> Pure Integer | Join [Free [] Integer]
	Free [] (Free [] Integer)
		=> Pure (Free [] Integer) | Join [Free [] (Free [] Integer)]

	Join . replicate 4 . Pure <$> Join (map Pure [1, 2, 3])
		=> Join . replicate 4 . Pure <$> Join [Pure 1, Pure 2, Pure 3]
		=> Join . replicate 4
			<$> Join [Pure (Pure 1), Pure (Pure 2), Pure (Pure 3)]
		=> Join <$> Join [
			Pure [Pure 1, Pure 1, Pure 1, Pure 1],
			Pure [Pure 2, Pure 2, Pure 2, Pure 2],
			Pure [Pure 3, Pure 3, Pure 3, Pure 3] ]
		=> Join [
			Pure $ Join [Pure 1, Pure 1, Pure 1, Pure 1],
			Pure $ Join [Pure 2, Pure 2, Pure 2, Pure 2],
			Pure $ Join [Pure 3, Pure 3, Pure 3, Pure 3] ]
	join' it
		=> Join $ join' <$> [
			Pure $ Join [Pure 1, Pure 1, Pure 1, Pure 1],
			Pure $ Join [Pure 2, Pure 2, Pure 2, Pure 2],
			Pure $ Join [Pure 3, Pure 3, Pure 3, Pure 3] ]
		=> Join [
			join' . Pure $ Join [Pure 1, Pure 1, Pure 1, Pure 1],
			join' . Pure $ Join [Pure 2, Pure 2, Pure 2, Pure 2],
			join' . Pure $ Join [Pure 3, Pure 3, Pure 3, Pure 3] ]
		=> Join [
			Join [Pure 1, Pure 1, Pure 1, Pure 1],
			Join [Pure 2, Pure 2, Pure 2, Pure 2],
			Join [Pure 3, Pure 3, Pure 3, Pure 3] ]
