{-# LANGUAGE FlexibleInstances #-}

module ChapterExercises where

--
--
-- Determine if a valid Functor can be written for the datatype provided.
--
--
-- 1. no, because "Bool" isn't a higher-kinded datatype.
--
-- 2. yes
--
-- 3. yes
--
-- 4. yes
--
-- 5. no
--
--
-- Rearrange the arguments to the type constructor of the datatype so the
-- Functor instance works.
--
data Sum a b
  = Second a
  | First b

-- Don't get confused by the "a" and "b" in the data constructors!
instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

data Company a b c
  = DeepBlue a
             b
  | Something c

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More a b
  = L b
      a
      b
  | R a
      b
      a
  deriving (Show, Eq)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

--
--
-- Write Functor instances for the following datatypes.
--
--
data Quant a b
  = Finance
  | Desk a
  | Bloor b
  deriving (Show, Eq)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- "b" is a phantom type!
newtype K a b =
  K a
  deriving (Show, Eq)

instance Functor (K a) where
  fmap _ (K a) = K a

newtype Flip f a b =
  Flip (f b a)
  deriving (Show, Eq)

newtype K2 a b =
  K2 a
  deriving (Show, Eq)

instance Functor (Flip K2 a) where
  fmap f (Flip (K2 b)) = Flip (K2 (f b))

newtype EvilGoateeConst a b =
  GoatyConst b
  deriving (Show, Eq)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

newtype LiftItOut f a =
  LiftItOut (f a)
  deriving (Show, Eq)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

data Parappa f g a =
  DaWrappa (f a)
           (g a)
  deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b =
  IgnoringSomething (f a)
                    (g b)
  deriving (Show, Eq)

-- "f" doesn't need to be a Functor because you're not mapping over it
-- (that's because it's part of the structure)
instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

data Notorious g o a t =
  Notorious (g o)
            (g a)
            (g t)
  deriving (Show, Eq)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Show, Eq)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

data TalkToMe a
  = Halt
  | Print String
          a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read sToA) = Read (f . sToA)
