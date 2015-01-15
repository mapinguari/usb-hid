-- consider writing parsers for physical descriptors
-- write documentation for all.
module System.USB.HID.Requests (-- * Request Types 
                                HIDDescriptorClass, HIDControlRequest, HIDProtocol, DescriptorIndex, HIDInterfaceNumber, Duration,ReportID, Report,ReportType, 
                                -- * HID Requests
                                -- | These are all specified in the HID Specification Section 7
                                getHIDReportDesc, getPhysicalDescriptor, getReport, setReport, getIdle, setIdle, getProtocol, setProtocol)where

import Data.Word
import Data.Bits.Bitwise hiding (map)
import System.USB
import Data.Attoparsec.ByteString
import System.USB.HID.Descriptor
import System.USB.HID.Parsers
import Data.ByteString as B (ByteString,empty,head)
import Data.Maybe (fromJust)
import Data.Tuple (swap)

data HIDDescriptorClass = HIDR
                        | HIDReportR 
                        | PhysicalDescriptorR
                          deriving (Eq,Show)

data HIDControlRequest = GetReport 
                       | GetIdle 
                       | GetProtocol 
                       | SetReport 
                       | SetIdle 
                       | SetProtocol
                         deriving (Eq,Show)

data HIDProtocol = Boot | Report 
                   deriving (Eq, Show,Enum)

instance Enum HIDControlRequest where 
 toEnum x = fromJust (lookup x (zip [1,2,3,9,10,11] [GetReport, GetIdle, GetProtocol, SetReport, SetIdle, SetProtocol]))
 fromEnum x = fromJust (lookup x (zip [GetReport, GetIdle, GetProtocol, SetReport, SetIdle, SetProtocol] [1,2,3,9,10,11]))

type DescriptorIndex = Word8

type HIDInterfaceNumber = Word16

type Duration = Word8

type ReportID = Word8

type Report = ByteString

data ReportType = RInput | ROutput | RFeature
                  deriving (Eq,Show)

instance Enum ReportType where 
    toEnum x = fromJust (lookup x as)
        where as = [(1,RInput),(2,ROutput),(3,RFeature)]
    fromEnum x = fromJust (lookup x (map swap as))
        where as = [(1,RInput),(2,ROutput),(3,RFeature)]

instance Enum HIDDescriptorClass where 
    toEnum 0x21 = HIDR
    toEnum 0x22 = HIDReportR 
    toEnum 0x23 = PhysicalDescriptorR
    fromEnum HIDR = 0x21
    fromEnum HIDReportR = 0x22
    fromEnum PhysicalDescriptorR = 0x23

getHIDDesc :: DeviceHandle -> Parser a -> HIDControlRequest -> HIDDescriptorClass -> DescriptorIndex -> HIDInterfaceNumber -> Size -> Timeout -> IO a 
getHIDDesc h parser cr dc di intN s t = do 
  (bs,s) <- readControl h Class ToInterface (toEnum . fromEnum $ cr) ((toEnum . fromEnum $ dc )*256 + (toEnum . fromEnum $ di)) intN s t
  case parseOnly parser bs of 
    Left x -> fail "Could not Parse descriptor"
    Right x -> return x

-- | Performs 'Class' request for the HID report descriptor using the Control pipe
getHIDReportDesc :: DeviceHandle -> HIDInterfaceNumber -> Size -> Timeout -> IO HIDReportDesc
getHIDReportDesc h intN = getHIDDesc h parseHIDReportDesc GetReport HIDReportR 0 intN 


-- | Performs 'Class' request for a HID Physical Descriptor set using the Control pipe
getPhysicalDescriptor :: DeviceHandle -> HIDInterfaceNumber -> DescriptorIndex -> Size -> Timeout -> IO HIDPhysDescSet
getPhysicalDescriptor h intN di = getHIDDesc h parsePhysDescSet GetReport PhysicalDescriptorR di intN 

-- | The GetReport request allows the host to receive a report via the Control pipe.

getReport :: DeviceHandle -> ReportType -> ReportID -> HIDInterfaceNumber -> Size -> Timeout -> IO (ByteString,Status)
getReport h rt ri intN s t = hidGet h (toEnum . fromEnum $ SetReport) ((toEnum . fromEnum $ rt )*256 + (toEnum . fromEnum $ ri)) intN s t

-- | The SetReport request allows the host to send a report to the device, possibly setting the state of input, output, or feature controls.

setReport :: DeviceHandle -> ReportType -> ReportID -> HIDInterfaceNumber -> Report -> Timeout -> IO (Size,Status)
setReport h rt ri intN r t = hidSet h (toEnum . fromEnum $ SetReport) ((toEnum . fromEnum $ rt )*256 + (toEnum . fromEnum $ ri)) intN r t

-- | The GetIdle request reads the current idle rate for a particular Input report

getIdle :: DeviceHandle -> HIDInterfaceNumber -> ReportID -> Timeout -> IO (HIDProtocol,Status)
getIdle h intN rid t = do 
  (p,s) <- hidGet h (toEnum . fromEnum $ GetIdle) (toEnum . fromEnum $ rid) intN 1 t
  let i = fromEnum (B.head p)
  return (toEnum i ,s)

-- | The SetIdle request silences a particular report on the Interrupt In pipe until a new event occurs or the specified amount of time passes.

setIdle :: DeviceHandle -> HIDInterfaceNumber -> Duration -> ReportID -> Timeout -> IO (Size,Status)
setIdle h intN d rid = hidSet h (toEnum . fromEnum $ SetIdle) ((toEnum . fromEnum $ d )*256 + (toEnum . fromEnum $ rid)) intN empty

-- | The GetProtocol request reads which protocol is currently active (either the boot protocol or the report protocol.)

getProtocol :: DeviceHandle -> HIDInterfaceNumber -> Timeout -> IO (HIDProtocol,Status)
getProtocol h intN t = do 
  (p,s) <- hidGet h (toEnum . fromEnum $ GetProtocol) 0 intN 1 t
  let i = fromEnum (B.head p)
  return (toEnum i ,s)

-- | The SetProtocol switches between the boot protocol and the report protocol (or vice versa).

setProtocol :: DeviceHandle -> HIDProtocol -> HIDInterfaceNumber -> Timeout -> IO (Size,Status)
setProtocol h p intN = hidSet h (toEnum . fromEnum $ SetProtocol) (toEnum . fromEnum $ p) intN empty

hidSet :: DeviceHandle -> Word8 -> Word16 -> Word16 -> ByteString ->Timeout -> IO (Size,Status)
hidSet h = writeControl h Class ToInterface 

hidGet :: DeviceHandle -> Word8 -> Word16 -> Word16 -> Size -> Timeout -> IO (ByteString,Status)
hidGet h = readControl h Class ToInterface

