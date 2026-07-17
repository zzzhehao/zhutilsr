msg <- c("Haploniscus sp. A" =
        "                              10000000000000000000000000                       
                    00000007 0      00      0    00   000000025                
             70000000  00    0      00      0    0   00        5700000000      
         000   00  20   00    00     00     0   70   0                     0002
       00      00   0    07     0     0    70   00  30                 0000    
      70       00   0     0     00    0    0                     00002         
      0        0    00    00     0   40                             00         
700000         0    00     0     0   00                              0         
 000005       70    00     0     0    0                           0 00         
      00       0    00    70    00   00              0             00000       
       04      00   00    00    0     0     03  00   0                   7000  
        0007   00   00   00    00    00     00   0  20                00000000 
            000009  0   00   707     07     07  70   00      000000000         
                  00000000   0      000     00   00   0000000                  
                          00070000000 00000007000000000                        
"
)

.onAttach <- function(libname, pkgname) {
    {
        set.seed(as.numeric(lubridate::today()+1))
        idx <- sample(length(msg), 1)
    }
    idx = 1
    art <- msg[idx]
    name <- names(msg)[idx]
    packageStartupMessage(art)
    packageStartupMessage(cli::cli_text(
        "\n\nThis is zhutilsr {packageVersion('zhutilsr')}. Zhehao's utilities, wrappers, and original algorithms. Welcome. \n\nToday's ACSII art: {name}\n\n"
    ))
    packageStartupMessage(
        "This package is under active development. If you find any bugs, please report at https://github.com/zzzhehao/zhutilsr/issues"
    )
    packageStartupMessage("Friendly reminder: isopods are no bugs.")
}
