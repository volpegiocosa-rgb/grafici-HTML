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

    palette = [ '#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33', &
                '#a65628','#f781bf','#999999','#66c2a5','#a6cee3','#b2df8a' ]

    is_plot = (trim(adjustl(plot_type)) == 'plot')

    if (present(xlabel)) then; xlab = xlabel; display_x = 'true'; else; xlab = ''; display_x = 'false'; end if
    if (present(ylabel)) then; ylab = ylabel; display_y = 'true'; else; ylab = ''; display_y = 'false'; end if
    showPoints = 'false'; if (is_plot .and. n < 20) showPoints = 'true'

    open(newunit=unit, file=filename, status='replace', action='write', form='formatted')

    ! === HTML STABILE E RESPONSIVE ===
    write(unit,'(A)') '<!DOCTYPE html>'
    write(unit,'(A)') '<html lang="it">'
    write(unit,'(A)') '<head>'
    write(unit,'(A)') '  <meta charset="utf-8">'
    write(unit,'(A)') '  <meta name="viewport" content="width=device-width, initial-scale=1">'
    write(unit,'(A)') '  <title>Grafico Stabile</title>'
    write(unit,'(A)') '  <style>'
    write(unit,'(A)') '    body { font-family: sans-serif; margin: 18px; padding: 0; }'
    write(unit,'(A)') '    .chart-container {'
    write(unit,'(A)') '      position: relative;'
    write(unit,'(A)') '      width: 100%;'
    write(unit,'(A)') '      height: 70vh;'
    write(unit,'(A)') '      max-width: 100%;'
    write(unit,'(A)') '      margin: 0 auto;'
    write(unit,'(A)') '      overflow: hidden;'
    write(unit,'(A)') '    }'
    write(unit,'(A)') '    canvas {'
    write(unit,'(A)') '      width: 100% !important;'
    write(unit,'(A)') '      height: 100% !important;'
    write(unit,'(A)') '      display: block;'
    write(unit,'(A)') '    }'
    write(unit,'(A)') '    button { margin: 6px; padding: 8px; }'
    write(unit,'(A)') '  </style>'
    write(unit,'(A)') '  <script src="https://cdn.jsdelivr.net/npm/chart.js@4.3.0/dist/chart.umd.min.js"></script>'
    write(unit,'(A)') '  <script src="https://cdn.jsdelivr.net/npm/chartjs-plugin-zoom@2.2.0/dist/chartjs-plugin-zoom.min.js"></script>'
    write(unit,'(A)') '</head>'
    write(unit,'(A)') '<body>'
    write(unit,'(A)') '  <h3>Grafico Stabile da Fortran</h3>'
    write(unit,'(A)') '  <div class="chart-container">'
    write(unit,'(A)') '    <canvas id="plot"></canvas>'
    write(unit,'(A)') '  </div>'
    write(unit,'(A)') '  <button onclick="resetZoom()">Reset Zoom</button>'
    write(unit,'(A)') '  <button onclick="savePNG()">Salva PNG</button>'
    write(unit,'(A)') '  <script>'

    ! === Palette ===
    write(unit,'(A)') '    const palette = ["#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628","#f781bf","#999999","#66c2a5","#a6cee3","#b2df8a"];'

    ! === Dati x ===
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

    ! === Etichette ===
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

    ! === Grafico STABILE ===
    if (is_plot) then
        write(unit,'(A)') '    const showPoints = '//showPoints//';'
        write(unit,'(A)') '    const datasets = [];'
        write(unit,'(A)') '    for(let k=0; k<y_series.length; k++) {'
        write(unit,'(A)') '      const pts = [];'
        write(unit,'(A)') '      for(let i=0; i<x_vals.length; i++) pts.push({x: x_vals[i], y: y_series[k][i]});'
        write(unit,'(A)') '      datasets.push({'
        write(unit,'(A)') '        label: series_labels[k],'
        write(unit,'(A)') '        data: pts,'
        write(unit,'(A)') '        borderColor: palette[k % palette.length],'
        write(unit,'(A)') '        backgroundColor: palette[k % palette.length],'
        write(unit,'(A)') '        showLine: true,'
        write(unit,'(A)') '        pointRadius: (showPoints ? 4 : 0),'
        write(unit,'(A)') '        tension: 0.1'
        write(unit,'(A)') '      });'
        write(unit,'(A)') '    }'
        write(unit,'(A)') '    const ctx = document.getElementById("plot").getContext("2d");'
        write(unit,'(A)') '    const myChart = new Chart(ctx, {'
        write(unit,'(A)') '      type: "scatter",'
        write(unit,'(A)') '      data: { datasets: datasets },'
        write(unit,'(A)') '      options: {'
        write(unit,'(A)') '        responsive: true,'
        write(unit,'(A)') '        maintainAspectRatio: false,'  ! CRITICO
        write(unit,'(A)') '        resizeDelay: 100,'           ! ANTI-TREMOLIO
        write(unit,'(A)') '        interaction: { mode: "nearest", intersect: false },'
        write(unit,'(A)') '        plugins: {'
        write(unit,'(A)') '          legend: { position: "top" },'
        write(unit,'(A)') '          zoom: {'
        write(unit,'(A)') '            zoom: { wheel: { enabled: true }, pinch: { enabled: true }, mode: "xy" },'
        write(unit,'(A)') '            pan: { enabled: true, mode: "xy" },'
        write(unit,'(A)') '            limits: { x: { min: "original", max: "original" }, y: { min: "original", max: "original" } }'
        write(unit,'(A)') '          }'
        write(unit,'(A)') '        },'
        write(unit,'(A)') '        scales: {'
        write(unit,'(A)') '          x: { title: { display: '//display_x//', text: "'//trim(xlab)//'" } },'
        write(unit,'(A)') '          y: { title: { display: '//display_y//', text: "'//trim(ylab)//'" } }'
        write(unit,'(A)') '        }'
        write(unit,'(A)') '      }'
        write(unit,'(A)') '    });'
    else
        ! Istogramma stabile
        write(unit,'(A)') '    function buildHistDatasets(nBins) {'
        write(unit,'(A)') '      const allVals = [].concat(...y_series);'
        write(unit,'(A)') '      const minV = Math.min(...allVals); const maxV = Math.max(...allVals);'
        write(unit,'(A)') '      const binW = (maxV - minV) / nBins; const edges = [];'
        write(unit,'(A)') '      for(let i=0; i<=nBins; i++) edges.push(minV + i * binW);'
        write(unit,'(A)') '      const labelsBins = [];'
        write(unit,'(A)') '      for(let i=0; i<nBins; i++) labelsBins.push(edges[i].toFixed(3) + " – " + edges[i+1].toFixed(3));'
        write(unit,'(A)') '      const datasets = [];'
        write(unit,'(A)') '      for(let k=0; k<y_series.length; k++) {'
        write(unit,'(A)') '        const counts = new Array(nBins).fill(0);'
        write(unit,'(A)') '        for(let v of y_series[k]) {'
        write(unit,'(A)') '          let idx = Math.floor((v - minV) / binW);'
        write(unit,'(A)') '          if(idx < 0) idx = 0; if(idx >= nBins) idx = nBins - 1;'
        write(unit,'(A)') '          counts[idx]++;'
        write(unit,'(A)') '        }'
        write(unit,'(A)') '        datasets.push({ label: series_labels[k], data: counts, backgroundColor: palette[k % palette.length] });'
        write(unit,'(A)') '      }'
        write(unit,'(A)') '      return { labels: labelsBins, datasets: datasets };'
        write(unit,'(A)') '    }'
        write(unit,'(A)') '    const histData = buildHistDatasets(10);'
        write(unit,'(A)') '    const ctx = document.getElementById("plot").getContext("2d");'
        write(unit,'(A)') '    const myChart = new Chart(ctx, {'
        write(unit,'(A)') '      type: "bar",'
        write(unit,'(A)') '      data: histData,'
        write(unit,'(A)') '      options: {'
        write(unit,'(A)') '        responsive: true,'
        write(unit,'(A)') '        maintainAspectRatio: false,'
        write(unit,'(A)') '        resizeDelay: 100,'
        write(unit,'(A)') '        plugins: {'
        write(unit,'(A)') '          legend: { position: "top" },'
        write(unit,'(A)') '          zoom: { zoom: { wheel: { enabled: true }, pinch: { enabled: true }, mode: "x" }, pan: { enabled: true, mode: "x" } }'
        write(unit,'(A)') '        }'
        write(unit,'(A)') '      }'
        write(unit,'(A)') '    });'
    end if

    ! === Funzioni ===
    write(unit,'(A)') '    function resetZoom() { if(myChart) myChart.resetZoom(); }'
    write(unit,'(A)') '    function savePNG() {'
    write(unit,'(A)') '      const a = document.createElement("a");'
    write(unit,'(A)') '      a.href = myChart.toBase64Image();'
    write(unit,'(A)') '      a.download = "grafico.png";'
    write(unit,'(A)') '      a.click();'
    write(unit,'(A)') '    }'

    write(unit,'(A)') '  </script>'
    write(unit,'(A)') '</body>'
    write(unit,'(A)') '</html>'

    close(unit)
    print *, 'Creato file HTML STABILE: ', trim(filename)

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
