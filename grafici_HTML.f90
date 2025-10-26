module html_plot_mod
  implicit none
contains

  subroutine write_xy_html(plot_type, x, y, n, m, filename, xlabel, ylabel, labels)
    implicit none
    character(len=*), intent(in) :: plot_type
    real(8), intent(in) :: x(:)
    real(8), intent(in) :: y(:,:)
    integer, intent(in) :: n, m
    character(len=*), intent(in) :: filename
    character(len=*), intent(in), optional :: xlabel, ylabel
    character(len=*), dimension(:), intent(in), optional :: labels
    integer :: i, j, unit
    character(len=512) :: buf, tmp
    character(len=256) :: xlab, ylab
    character(len=32) :: display_x, display_y, showPoints
    logical :: is_plot
    character(len=32), dimension(12) :: palette
    character(len=512) :: safe_label

    ! Palette
    palette = [ '#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33', &
                '#a65628','#f781bf','#999999','#66c2a5','#a6cee3','#b2df8a' ]

    is_plot = (trim(adjustl(plot_type)) == 'plot')

    ! Etichette assi
    if (present(xlabel)) then
        xlab = xlabel
        display_x = 'true'
    else
        xlab = ''
        display_x = 'false'
    end if
    if (present(ylabel)) then
        ylab = ylabel
        display_y = 'true'
    else
        ylab = ''
        display_y = 'false'
    end if

    showPoints = 'false'
    if (is_plot .and. n < 20) showPoints = 'true'

    open(newunit=unit, file=filename, status='replace', action='write', form='formatted')

    ! === HTML Header ===
    write(unit,'(A)') '<!DOCTYPE html>'
    write(unit,'(A)') '<html lang="it">'
    write(unit,'(A)') '<head>'
    write(unit,'(A)') '  <meta charset="utf-8">'
    write(unit,'(A)') '  <meta name="viewport" content="width=device-width, initial-scale=1">'
    write(unit,'(A)') '  <title>Grafico interattivo</title>'
    write(unit,'(A)') '  <style>body{font-family:sans-serif;margin:18px}canvas{max-width:100%;height:70vh}button{margin:6px;padding:8px}</style>'
    write(unit,'(A)') '  <script src="https://cdn.jsdelivr.net/npm/chart.js@4.3.0/dist/chart.umd.min.js"></script>'
    write(unit,'(A)') '  <script src="https://cdn.jsdelivr.net/npm/chartjs-plugin-zoom@2.2.0/dist/chartjs-plugin-zoom.min.js"></script>'
    write(unit,'(A)') '</head>'
    write(unit,'(A)') '<body>'
    write(unit,'(A)') '  <canvas id="plot" width="800" height="600"></canvas><br>'
    write(unit,'(A)') '  <button onclick="resetZoom()">Reset Zoom</button>'
    write(unit,'(A)') '  <button onclick="savePNG()">Salva PNG</button>'
    write(unit,'(A)') '  <script>'

    ! === Palette JS (una riga per colore, ma senza spazi extra) ===
    write(unit,'(A)') '    const palette = ['// &
                      '"#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33", '// &
                      '"#a65628","#f781bf","#999999","#66c2a5","#a6cee3","#b2df8a"'// &
                      '];'

    ! === Dati x (una riga per valore, senza spazi iniziali) ===
    write(unit,'(A)', advance='no') '    const x_vals = ['
    do i = 1, n
        write(buf,'(F0.8)') x(i)
        write(unit,'(A)', advance='no') trim(adjustl(buf))
        if (i < n) write(unit,'(A)', advance='no') ','
    end do
    write(unit,'(A)') '];'

    ! === Dati y ===
    write(unit,'(A)') '    const y_series = [];'
    do j = 1, m
        write(unit,'(A)', advance='no') '    y_series.push(['
        do i = 1, n
            write(buf,'(F0.8)') y(i,j)
            write(unit,'(A)', advance='no') trim(adjustl(buf))
            if (i < n) write(unit,'(A)', advance='no') ','
        end do
        write(unit,'(A)') ']);'
    end do

    ! === Etichette serie ===
    write(unit,'(A)') '    const series_labels = [];'
    do j = 1, m
        if (present(labels) .and. size(labels) >= j) then
            safe_label = escape_quotes(labels(j))
        else
            write(tmp,'("Serie ",I0)') j
            safe_label = trim(tmp)
        end if
        write(unit,'(A)') '    series_labels.push("'//trim(safe_label)//'");'
    end do

    ! === Grafico (scatter o bar) ===
    if (is_plot) then
        write(unit,'(A)') '    const showPoints = '//showPoints//';'
        write(unit,'(A)') '    const datasets = [];'
        write(unit,'(A)') '    for(let k=0;k<y_series.length;k++){'
        write(unit,'(A)') '      const pts=[]; for(let i=0;i<x_vals.length;i++) pts.push({x:x_vals[i],y:y_series[k][i]});'
        write(unit,'(A)') '      datasets.push({label:series_labels[k], data:pts, borderColor:palette[k%palette.length], '// &
                          'backgroundColor:palette[k%palette.length], showLine:true, pointRadius:(showPoints?4:0), tension:0.1});'
        write(unit,'(A)') '    }'
        write(unit,'(A)') '    const ctx = document.getElementById("plot").getContext("2d");'
        write(unit,'(A)') '    const myChart = new Chart(ctx,{type:"scatter",data:{datasets:datasets},options:{'// &
                          'responsive:true,interaction:{mode:"nearest",intersect:false},plugins:{legend:{position:"top"},'// &
                          'zoom:{zoom:{wheel:{enabled:true},pinch:{enabled:true},mode:"xy"},pan:{enabled:true,mode:"xy"}}},'// &
                          'scales:{x:{title:{display:'//display_x//',text:"'//trim(xlab)//'"}},y:{title:{display:'//display_y// &
                          ',text:"'//trim(ylab)//'"}}}}});'
    else
        ! Istogramma
        write(unit,'(A)') '    function buildHistDatasets(nBins){'
        write(unit,'(A)') '      const allVals = [].concat(...y_series);'
        write(unit,'(A)') '      const minV=Math.min(...allVals); const maxV=Math.max(...allVals);'
        write(unit,'(A)') '      const binW=(maxV-minV)/nBins; const edges=[]; for(let i=0;i<=nBins;i++) edges.push(minV+i*binW);'
        write(unit,'(A)') '      const labelsBins=[]; for(let i=0;i<nBins;i++) labelsBins.push(edges[i].toFixed(3)+" – "+edges[i+1].toFixed(3));'
        write(unit,'(A)') '      const datasets=[];'
        write(unit,'(A)') '      for(let k=0;k<y_series.length;k++){'
        write(unit,'(A)') '        const counts=new Array(nBins).fill(0);'
        write(unit,'(A)') '        for(let v of y_series[k]){ let idx=Math.floor((v-minV)/binW); if(idx<0) idx=0; if(idx>=nBins) idx=nBins-1; counts[idx]++; }'
        write(unit,'(A)') '        datasets.push({label:series_labels[k], data:counts, backgroundColor:palette[k%palette.length]});'
        write(unit,'(A)') '      }'
        write(unit,'(A)') '      return {labels:labelsBins,datasets:datasets};'
        write(unit,'(A)') '    }'
        write(unit,'(A)') '    const histData=buildHistDatasets(10);'
        write(unit,'(A)') '    const ctx=document.getElementById("plot").getContext("2d");'
        write(unit,'(A)') '    const myChart=new Chart(ctx,{type:"bar",data:histData,options:{'// &
                          'responsive:true,plugins:{legend:{position:"top"},'// &
                          'zoom:{zoom:{wheel:{enabled:true},pinch:{enabled:true},mode:"x"},pan:{enabled:true,mode:"x"}}}}});'
    end if

    ! === Funzioni ===
    write(unit,'(A)') '    function resetZoom(){ if(myChart) myChart.resetZoom(); }'
    write(unit,'(A)') '    function savePNG(){ const a=document.createElement("a"); a.href=myChart.toBase64Image(); a.download="grafico.png"; a.click(); }'

    write(unit,'(A)') '  </script>'
    write(unit,'(A)') '</body>'
    write(unit,'(A)') '</html>'

    close(unit)
    print *, 'Creato file HTML: ', trim(filename)

  contains

    function escape_quotes(str) result(res)
      character(len=*), intent(in) :: str
      character(len=512) :: res
      integer :: i
      res = ''
      do i = 1, len_trim(str)
        if (str(i:i) == "'") then
          res = trim(res) // "''"
        else if (str(i:i) == '"') then
          res = trim(res) // '&quot;'
        else
          res = trim(res) // str(i:i)
        end if
      end do
    end function escape_quotes

  end subroutine write_xy_html

end module html_plot_mod
