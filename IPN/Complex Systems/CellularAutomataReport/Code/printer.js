
values = [54, 106, 110]
for (val of values) {
data = 
            `
            ${val == values[0]? "" : "\\clearpage"}
            \\subsection{${val}}
            \\begin{figure}[ht!]
              \\centering
              \\begin{subfigure}[b]{0.4\\linewidth}
                \\includegraphics[width=0.6\\textwidth]{Images/${val}/a.png}
                \\caption{One}
              \\end{subfigure}
              \\begin{subfigure}[b]{0.4\\linewidth}
                \\includegraphics[width=0.6\\textwidth]{Images/${val}/b.png}
                \\caption{8\\%}
              \\end{subfigure}
              \\begin{subfigure}[b]{0.4\\linewidth}
                \\includegraphics[width=0.6\\textwidth]{Images/${val}/c.png}
                \\caption{50\\%}
              \\end{subfigure}
              \\begin{subfigure}[b]{0.4\\linewidth}
                \\includegraphics[width=0.6\\textwidth]{Images/${val}/d.png}
                \\caption{87\\%}
              \\end{subfigure}
            \\end{figure}

            \\begin{figure}[ht!]
              \\centering
              \\begin{subfigure}[b]{0.4\\linewidth}
                \\includegraphics[width=0.6\\textwidth]{Images/${val}/dia-a.png}
                \\caption{One}
              \\end{subfigure}
              \\begin{subfigure}[b]{0.4\\linewidth}
                \\includegraphics[width=0.6\\textwidth]{Images/${val}/dia-b.png}
                \\caption{8\\%}
              \\end{subfigure}
              \\begin{subfigure}[b]{0.4\\linewidth}
                \\includegraphics[width=0.6\\textwidth]{Images/${val}/dia-c.png}
                \\caption{50\\%}
              \\end{subfigure}
              \\begin{subfigure}[b]{0.4\\linewidth}
                \\includegraphics[width=0.6\\textwidth]{Images/${val}/dia-d.png}
                \\caption{87\\%}
              \\end{subfigure}
            \\end{figure}
`

            console.log(data)
}

