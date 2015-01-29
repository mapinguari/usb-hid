{-# LANGUAGE DeriveGeneric , AutoDeriveTypeable  #-}
-- | Notice that 'LongItem's are not supported as none are specified on the <http://www.usb.org/developers/hidpage/ USB Human Interface device page>
module System.USB.HID.Descriptor where 


import Data.ByteString as B
import Data.Word 
import GHC.Generics

data HIDDescriptor = HIDDescriptor 
    {hIDDescriptorType :: !Word8,
     hIDcdHID :: !Version,
     hIDCountryCode :: !Word8,
     hIDNumDesc :: !Word8,
     hIDDescType :: !Word8,
     hIDDescLength :: !Int,
     hIDDescType' :: !(Maybe Word8),
     hIDDescLength' :: !(Maybe Word8)
    } deriving (Show,Eq,Generic)

newtype HIDReportDesc = HIDReport [HIDReportItem]
                 deriving (Eq,Show,Generic)

data HIDPhysicalDescriptor = PD !HIDDesignator !HIDQualifier !HIDEffort
                             deriving (Eq,Show,Generic)

data HIDReportItem = HIDReportS !ShortItem 
                   | HIDReportL !LongItem
                     deriving (Eq)

instance Show HIDReportItem where 
    show (HIDReportS x) = show x
    show (HIDReportL x) = show x 

data ShortItem = Main !HIDMainTag
               | Global !HIDGlobalTag
               | Local !HIDLocalTag
                 deriving (Eq,Generic)

instance Show ShortItem where 
    show (Main x) = show x
    show (Global x) = show x 
    show (Local x) = show x

newtype LongItem = Long ()
                deriving (Eq,Show,Generic)

data HIDMainTag = Input !HIDMainData 
                | Output !HIDMainData 
                | Feature !HIDMainData
                | Collection !HIDCollectionData
                | EndCollection
                  deriving (Show,Eq,Generic)

--Name these more meaningfully
data HIDMainData = HIDMainData 
    {bit0 :: !HDMBit0,
     bit1 :: !HDMBit1,
     bit2 :: !HDMBit2,
     bit3 :: !HDMBit3,
     bit4 :: !HDMBit4,
     bit5 :: !HDMBit5,
     bit6 :: !HDMBit6,
     bit8 :: !HDMBit8
    } deriving (Show,Eq,Generic)

constructHIDMainD :: [Int] -> HIDMainData
constructHIDMainD [a,b,c,d,e,f,g,h] = HIDMainData (toEnum a) (toEnum b) (toEnum c) (toEnum d) (toEnum e) (toEnum f) (toEnum g) (toEnum h)

data HDMBit0 = Data | Constant
               deriving (Eq,Show,Enum,Generic)
data HDMBit1 = Array | Variable
               deriving (Eq,Show,Enum,Generic)
data HDMBit2 = Absolute | Relative
               deriving (Eq,Show,Enum,Generic)
data HDMBit3 = NoWrap | Wrap
               deriving (Eq,Show,Enum,Generic)
data HDMBit4 = Linear | NonLinear
               deriving (Eq,Show,Enum,Generic)
data HDMBit5 = PreferredState | NoPreferred
               deriving (Eq,Show,Enum,Generic)
data HDMBit6 = NoNullPosition | NullState
               deriving (Eq,Show,Enum,Generic)
data HDMBit8 = BitField | BufferedBytes
               deriving (Eq,Show,Enum,Generic)

data HIDCollectionData = Physical 
                       | Application
                       | Logical
                       | Report
                       | NamedArray 
                       | UsageSwitch
                       | UsageModifier
                         deriving (Eq,Show,Enum,Generic)

data HIDGlobalTag = UsagePage !HIDUsagePage
                  | LogicalMinimum !Int
                  | LogicalMaximum !Int
                  | PhysicalMinimum !Int
                  | PhysicalMaximum !Int
                  | UnitExponent !Int
                  | Unit !Int
                  | ReportSize !Int
                  | ReportID !Int
                  | ReportCount !Int
                  | Push !Int
                  | Pop !Int 
                    deriving (Show,Eq,Generic)

data HIDLocalTag = Usage !HIDUsage
                 | UsageMinimum !Int
                 | UsageMaximum !Int
                 | DesignatorIndex !HIDDesignator
                 | DesignatorMinimum !Int
                 | DesignatorMaximum !Int
                 | StringIndex !Int
                 | StringMinimum !Int
                 | StringMaximum !Int
                 | Delimiter !HIDDelimeter
                   deriving (Show,Eq,Generic)

data HIDDelimeter = Open | Close
                    deriving (Show,Eq,Enum,Generic)

newtype HIDUsagePage = UP Int
    deriving (Show,Eq,Generic)
             
newtype HIDUsage = U Int
    deriving (Show,Eq,Generic)

data HIDDesignator = None
                   | Hand
                   | Eyeball
                   | Eyebrow
                   | Eyelid
                   | Ear
                   | Nose
                   | Mouth
                   | UpperLip
                   | LowerLip
                   | Jaw
                   | Neck
                   | UpperArm
                   | Elbow
                   | Forearm
                   | Wrist
                   | Palm
                   | Thumb
                   | IndexFinger
                   | MiddleFinger
                   | RingFinger
                   | LittleFinger
                   | Head
                   | Shoulder
                   | Hip
                   | Waist
                   | Thigh
                   | Knee
                   | Calf
                   | Ankle
                   | Foot
                   | Heel
                   | BallOfFoot
                   | BigToe
                   | SecondToe
                   | ThirdToe
                   | FourthToe
                   | LittleToe
                   | Brow
                   | Cheek
                     deriving (Show,Eq,Enum,Generic)

data HIDQualifier = QNotApplicable
                  | RightSide
                  | LeftSide
                  | BothSides
                  | Either
                  | Center
                    deriving (Eq,Show,Enum,Generic)

data HIDBias = BNotApplicable
             | RightHand
             | LeftHand
             | BothHands
             | EitherHand
               deriving (Eq,Show,Enum,Generic)

data HIDPhysDescSet = PDS !HIDBias !HIDPreference ![HIDPhysicalDescriptor]
                    deriving (Eq,Show,Generic)

type HIDEffort = Int

type HIDPreference = Int


data Version = V {main :: !Int,
                  minor :: !Int
                 }
             deriving (Eq,Generic)

instance Show Version where 
    show (V ma mi) = (show ma) ++ "." ++ (show mi)
