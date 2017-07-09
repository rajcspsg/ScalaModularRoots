name := "ScalaModularRoots"

version := "1.0"

scalaVersion := "2.12.1"

lazy val signaturesAsTraitSettings = Seq(name := "SignatureAsTraits", version := "1.0", scalaVersion := "2.12.1")

lazy val signaturesAsTraitsProject = (project in file("signaturesAsTraits")).settings(signaturesAsTraitSettings)
        