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
                   ".ads"   //-- ADA
                  ].concat(imageExts);

module.exports = filesFilter;
