// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Compiler.StringResources

type SR() =
    static member lexOutsideIntegerRange() =
        (1, "This number is outside the allowable range for 32-bit signed integers")

    static member lexOutsideEightBitSigned() =
        (1, "This number is outside the allowable range for 8-bit signed integers")

    static member lexOutsideEightBitSignedHex() =
        (1, "This number is outside the allowable range for hexadecimal 8-bit signed integers")

    static member lexOutsideEightBitUnsigned() =
        (1, "This number is outside the allowable range for 8-bit unsigned integers")

    static member lexOutsideSixteenBitSigned() =
        (1, "This number is outside the allowable range for 16-bit signed integers")

    static member lexOutsideSixteenBitUnsigned() =
        (1, "This number is outside the allowable range for 16-bit unsigned integers")

    static member lexOutsideThirtyTwoBitSigned() =
        (1, "This number is outside the allowable range for 32-bit signed integers")

    static member lexOutsideThirtyTwoBitUnsigned() =
        (1, "This number is outside the allowable range for 32-bit unsigned integers")

    static member lexOutsideSixtyFourBitSigned() =
        (1, "This number is outside the allowable range for 64-bit signed integers")

    static member lexOutsideSixtyFourBitUnsigned() =
        (1, "This number is outside the allowable range for 64-bit unsigned integers")

    static member lexOutsideNativeSigned() =
        (1, "This number is outside the allowable range for signed native integers")

    static member lexOutsideNativeUnsigned() =
        (1, "This number is outside the allowable range for unsigned native integers")

    static member lexOutsideDecimal() =
        (1, "This number is outside the allowable range for decimal")

    static member lexOutsideThirtyTwoBitFloat() =
        (1, "This number is outside the allowable range for 32-bit float")

    static member lexInvalidFloat() = (1, "The number was an invalid float")
