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
    character(len=256) :: xlab, ylab, display_x, display_y, showPoints
    logical :: is_plot
    character(len=32), dimension(12) :: palette

    palette = [ '#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33', &
                '#a65628','#f781bf','#999999','#66c2a5','#a6cee3','#b2df8a' ]

    is_plot = (trim(adjustl(plot_type)) == 'plot')

    ! opzionali
    if (present(xlabel)) then
        xlab = trim(xlabel)
        display_x = 'true'
    else
        xlab = ''
        display_x = 'false'
    end if

    if (present(ylabel)) then
        ylab = trim(ylabel)
        display_y = 'true'
    else
        ylab = ''
        display_y = 'false'
    end if

    ! showPoints se n<20
    if (is_plot) then
        if (n < 20) then
            showPoints = 'true'
        else
            showPoints = 'false'
        end if
    end if

    open(newunit=unit, file=filename, status='replace', action='write', form='formatted')

    write(unit,*) '<!doctype html>'
    write(unit,*) '<html lang="it">'
    write(unit,*) '<head>'
    write(unit,*) '  <meta charset="utf-8">'
    write(unit,*) '  <meta name="viewport" content="width=device-width, initial-scale=1">'
    write(unit,*) '  <title>Grafico interattivo</title>'
    write(unit,*) '  <style>body{font-family:sans-serif;margin:18px} canvas{max-width:100%;height:70vh} button{margin:6px;padding:8px}</style>'
    write(unit,*) '  <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>'
    write(unit,*) '  <script src="https://cdn.jsdelivr.net/npm/chartjs-plugin-zoom@2.0.1"></script>'
    write(unit,*) '</head>'
    write(unit,*) '<body>'
    write(unit,*) '  <h3>Grafico generato da Fortran</h3>'
    write(unit,*) '  <canvas id="plot"></canvas><br>'
    write(unit,*) '  <button onclick="resetZoom()">🔄 Reset Zoom</button>'
    write(unit,*) '  <button onclick="savePNG()">💾 Salva PNG</button>'
    write(unit,*) '  <script>'

    ! palette JS
    write(unit,*) '    const palette = ['
    do i = 1,12
        buf = '      "' // trim(palette(i)) // '"'
        if (i<12) buf = buf // ','
        write(unit,*) buf
    end do
    write(unit,*) '    ];'

    ! x array
    write(unit,*) '    const x_vals = ['
    do i = 1,n
        write(buf,'(F0.8)') x(i)
        if (i<n) then
            buf = '      '//trim(buf)//',' 
        else 
            buf = '      '//trim(buf)
        end if
        write(unit,*) buf
    end do
    write(unit,*) '    ];'

    ! y arrays
    write(unit,*) '    const y_series = [];'
    do j = 1,m
        write(unit,*) '    y_series.push(['
        do i = 1,n
            write(buf,'(F0.8)') y(i,j)
            if (i<n) then
                buf = '      '//trim(buf)//',' 
            else 
                buf = '      '//trim(buf)
            end if
            write(unit,*) buf
        end do
        write(unit,*) '    ]);'
    end do

    ! labels JS
    write(unit,*) '    const series_labels = [];'
    do j = 1,m
        if (present(labels)) then
            if (size(labels) >= j) then
                tmp = trim(labels(j))
            else
                write(tmp,'("Serie ",I0)') j
            end if
        else
            write(tmp,'("Serie ",I0)') j
        end if
        buf = '    series_labels.push("'//trim(tmp)//'");'
        write(unit,*) buf
    end do

    ! chart build
    if (is_plot) then
        write(unit,*) '    const chartConfig = {'
        write(unit,*) '      type: "scatter",'
        write(unit,*) '      data: { datasets: [] },'
        write(unit,*) '      options: {'
        write(unit,*) '        responsive:true,'
        write(unit,*) '        interaction:{mode:"nearest",intersect:false},'
        write(unit,*) '        plugins:{ legend:{position:"top"}, zoom:{ zoom:{ wheel:{enabled:true}, pinch:{enabled:true}, mode:"xy"}, pan:{enabled:true, mode:"xy"} } },'
        write(unit,*) '        scales:{ x:{ title:{ display:'//trim(display_x)//', text:"'//trim(xlab)//'"} }, y:{ title:{ display:'//trim(display_y)//', text:"'//trim(ylab)//'"}} }'
        write(unit,*) '      }'
        write(unit,*) '    };'
        write(unit,*) '    const ctx = document.getElementById("plot").getContext("2d");'
        write(unit,*) '    const myChart = new Chart(ctx, chartConfig);'

        ! datasets
        write(unit,*) '    const showPoints = '//trim(showPoints)//';'
        write(unit,*) '    for(let k=0;k<y_series.length;k++){'
        write(unit,*) '      const pts=[]; for(let i=0;i<x_vals.length;i++) pts.push({x:x_vals[i],y:y_series[k][i]});'
        write(unit,*) '      myChart.data.datasets.push({label:series_labels[k], data:pts, borderColor:palette[k%palette.length], backgroundColor:palette[k%palette.length], showLine:true, pointRadius:(showPoints?4:0), tension:0.1});'
        write(unit,*) '    }'
        write(unit,*) '    myChart.update();'

    else
        ! histogram
        write(unit,*) '    function buildHistDatasets(nBins){'
        write(unit,*) '      const allVals = [].concat(...y_series);'
        write(unit,*) '      const minV=Math.min(...allVals); const maxV=Math.max(...allVals);'
        write(unit,*) '      const binW=(maxV-minV)/nBins; const edges=[]; for(let i=0;i<=nBins;i++) edges.push(minV+i*binW);'
        write(unit,*) '      const labelsBins=[]; for(let i=0;i<nBins;i++) labelsBins.push(edges[i].toFixed(3)+" – "+edges[i+1].toFixed(3));'
        write(unit,*) '      const datasets=[];'
        write(unit,*) '      for(let k=0;k<y_series.length;k++){'
        write(unit,*) '        const counts=new Array(nBins).fill(0);'
        write(unit,*) '        for(let v of y_series[k]){ let idx=Math.floor((v-minV)/binW); if(idx<0) idx=0;if(idx>=nBins) idx=nBins-1; counts[idx]++; }'
        write(unit,*) '        datasets.push({label:series_labels[k], data:counts, backgroundColor:palette[k%palette.length]});'
        write(unit,*) '      }'
        write(unit,*) '      return {labels:labelsBins,datasets:datasets};'
        write(unit,*) '    }'
        write(unit,*) '    const histData=buildHistDatasets(10);'
        write(unit,*) '    const chartConfig={type:"bar", data:histData, options:{responsive:true, plugins:{legend:{position:"top"}, zoom:{ zoom:{ wheel:{enabled:true}, pinch:{enabled:true}, mode:"x"}, pan:{enabled:true, mode:"x"} } }}};'
        write(unit,*) '    const ctx=document.getElementById("plot").getContext("2d");'
        write(unit,*) '    const myChart=new Chart(ctx, chartConfig);'
    end if

    ! reset & save
    write(unit,*) '    function resetZoom(){ if(myChart) myChart.resetZoom(); }'
    write(unit,*) '    function savePNG(){ const a=document.createElement("a"); a.href=myChart.toBase64Image(); a.download="grafico.png"; a.click(); }'

    write(unit,*) '  </script>'
    write(unit,*) '</body>'
    write(unit,*) '</html>'

    close(unit)
    print *, 'Creato file HTML: ', trim(filename)

  end subroutine write_xy_html

end module html_plot_mod
