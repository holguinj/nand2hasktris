module Machine.Emulator where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IMap
import           Data.Maybe  (fromMaybe)
import           Data.Vector (Vector, empty, (!?), (//))

type Byte = Int
type ByteVec = Vector Byte
type Memory = IntMap Byte
type Program = ByteVec

newtype Address = Address Byte

data Machine = Machine { aReg    :: Byte
                       , dReg    :: Byte
                       , memory  :: Memory
                       , pc      :: Byte
                       , program :: Program
                       }

emptyMachine :: Machine
emptyMachine = Machine { aReg = zero
                       , dReg = zero
                       , memory = emptyMem
                       , pc = zero
                       , program = empty
                       }

zero :: Byte
zero = 0

memGet :: Machine -> Address -> Byte
memGet Machine{memory=mem} (Address addr) = IMap.findWithDefault zero addr mem

memSet :: Machine -> Address -> Byte -> Machine
memSet machine@Machine{memory=mem} (Address addr) val = machine { memory = IMap.insert addr val mem }

mReg :: Machine -> Byte
mReg machine = memGet machine (Address $ aReg machine)

setMReg :: Machine -> Byte -> Machine
setMReg machine@Machine{aReg=reg} = memSet machine (Address reg)

emptyMem :: Memory
emptyMem = IMap.empty

pcInc :: Machine -> Machine
pcInc machine = machine { pc = pc machine + 1 }
