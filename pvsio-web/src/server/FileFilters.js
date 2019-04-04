var imageExts = [".jpg", ".jpeg", ".png"],
    filesFilter = [".pvs",  //-- PVS
                   ".muz",  //-- PIM
                   ".tex",  //-- PIM/Z
                   ".txt",
                   ".i",    //-- IVY
                   ".json",
                   ".emdl", //-- Emucharts
                   ".vdmsl",
                   ".aadl", //-- Bless/AADL
                   ".adb",  //-- ADA
                   ".ads",  //-- ADA
                   ".c",    //-- C
                   ".h",    //-- C
                   ".smv",  //-- NuXMV
                   ".als"   //-- Alloy
                  ].concat(imageExts);

module.exports = filesFilter;
