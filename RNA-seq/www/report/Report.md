---
title       : RNA-seq Vis Report
subtitle    : 
author      : 
job         : 
framework   : revealjs        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
revealjs:
  theme: solarized
  transition: none
  center: "true"
hitheme     : simple     # 
widgets     : mathjax            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

<style type="text/css">
body {background:none transparent;
}
</style>

<script language="javascript" type="text/javascript">
  function resizeIframe(obj) {
    obj.style.height = obj.contentWindow.document.body.scrollHeight + 'px';
  }
</script>

# RNA-seq Vis report
<small> Created by [RNA-seq Vis team](https://yuanhangliu.shinyapps.io/RNA-seq)</small>

---

## Data table

$$V_t(S_t) = \max_{x_t \in \chi_t} \left(C(S_t, x_t) + 
            \gamma \sum_{s^{\prime} \in \mathcal{S}} \mathbb{P}(s^{\prime} | S_t^n, x_t) V_{t+1}^{n-1} s^{\prime} \right)$$

<iframe src="htmlFiles/datatable.html" width=1000px height=1000px allowtransparency="true"> </iframe>

---

## Heatmap

<iframe src="htmlFiles/heatmap.html" width=1000px height=1000px allowtransparency="true"> </iframe>

---

## Density plot
<iframe src="htmlFiles/density.html" width=1000px height=1000px allowtransparency="true"> </iframe>

---

## Scatter plot

<iframe src="htmlFiles/Scatterplot.html" width=1000px height=1000px allowtransparency="true"> </iframe>

---

## Box plot

<iframe src="htmlFiles/Boxplot.html" width=1000px height=1000px allowtransparency="true"> </iframe>

---

## Principal component

<iframe src="htmlFiles/pcaplot.html" width=1000px height=1000px allowtransparency="true"> </iframe>

---

## Gene-gene interaction

<iframe src="htmlFiles/forceNet.html" width=1000px height=1000px allowtransparency="true"> </iframe>

---

## DE data table

<iframe src="htmlFiles/DEdatatable.html" width=1000px height=1000px allowtransparency="true"> </iframe>

---

## DE heatmap

<iframe src="htmlFiles/DEheatmap.html" width=1000px height=1000px allowtransparency="true"> </iframe>

---

## MA plot

<iframe src="htmlFiles/MAplot.html" width=1000px height=1000px allowtransparency="true"> </iframe>

---

## Dispersion plot

<iframe src="htmlFiles/DispersionPlot.html" width=1000px height=1000px allowtransparency="true"> </iframe>

