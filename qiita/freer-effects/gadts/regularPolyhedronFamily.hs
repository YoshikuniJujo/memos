{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RegularPolyhedron where

class SurfaceAreable sa where
	data FaceArea sa
	calcFaceArea :: sa -> FaceArea sa
	getSurfaceArea :: FaceArea sa -> Double

data Tetrahedron = Tetrahedron Integer deriving Show

instance SurfaceAreable Tetrahedron where
	data FaceArea Tetrahedron = FaceAreaTetra Double deriving Show
	calcFaceArea (Tetrahedron a) =
		FaceAreaTetra $ fromIntegral (a * a) * sin (pi / 3) / 2
	getSurfaceArea (FaceAreaTetra fa) = 4 * fa

data Hexahedron = Hexahedron Integer deriving Show

instance SurfaceAreable Hexahedron where
	data FaceArea Hexahedron = FaceAreaHexa Integer deriving Show
	calcFaceArea (Hexahedron a) = FaceAreaHexa $ a * a
	getSurfaceArea (FaceAreaHexa fa) = 6 * fromInteger fa

data Octahedron = Octahedron Integer deriving Show

instance SurfaceAreable Octahedron where
	data FaceArea Octahedron = FaceAreaOcta Double deriving Show
	calcFaceArea (Octahedron a) =
		FaceAreaOcta $ fromIntegral (a * a) * sin (pi / 3) / 2
	getSurfaceArea (FaceAreaOcta fa) = 8 * fa

data Dodecahedron = Dodecahedron Integer deriving Show

instance SurfaceAreable Dodecahedron where
	data FaceArea Dodecahedron = FaceAreaDodeca Double deriving Show
	calcFaceArea (Dodecahedron a) =
		FaceAreaDodeca $ fromIntegral (a * a) * 5 / (4 * tan (pi / 5))
	getSurfaceArea (FaceAreaDodeca fa) = 12 * fa

data Icosahedron = Icosahedron Integer deriving Show

instance SurfaceAreable Icosahedron where
	data FaceArea Icosahedron = FaceAreaIcosa Double deriving Show
	calcFaceArea (Icosahedron a) =
		FaceAreaIcosa $ fromIntegral (a * a) * sin (pi / 3) / 2
	getSurfaceArea (FaceAreaIcosa fa) = 20 * fa
