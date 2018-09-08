module Machine.Emulator where

import           Control.Applicative ((<|>))
import           Data.Maybe          (fromMaybe)
import           Data.Vector         ((!?), (//))
import qualified Data.Vector         as V

type Byte = Int
type ByteVec = V.Vector Byte
type Memory = ByteVec
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
                       , program = V.empty
                       }

zero :: Byte
zero = 0

memGet :: Machine -> Address -> Byte
memGet Machine{memory=mem} (Address addr) = fromMaybe zero $ mem !? addr

memSet :: Machine -> Address -> Byte -> Machine
memSet machine@Machine{memory=mem} (Address addr) val = machine { memory = mem // [(addr, val)] }

mReg :: Machine -> Byte
mReg machine = memGet machine (Address $ aReg machine)

setMReg :: Machine -> Byte -> Machine
setMReg machine@Machine{aReg=reg} = memSet machine (Address reg)

emptyMem :: ByteVec
emptyMem = V.replicate (32 * 1024) zero

pcInc :: Machine -> Machine
pcInc machine = machine { pc = pc machine + 1}
