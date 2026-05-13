<!-- # MARS -->
  
  <!-- ## [M]{.orange}ultivariate [A]{.orange}daptive [R]{.orange}egression [S]{.orange}plines -->
  
  <!-- ::: columns -->
  <!-- ::: {.column width="40%"} -->
  <!-- ![](img/mars.png) -->
  <!-- ::: -->
  
  <!-- ::: {.column width="60%"} -->
  <!-- -   MARS are a generalization of GAMs that avoid the [additivity -->
                                                               <!--     assumption]{.orange}. -->
  
  <!-- -   MARS allow modeling of [non--linear interactions]{.blue} and not -->
  <!--     just non--linear marginal effects. -->
  
  <!-- -   MARS are at the same time: -->
  
  <!--     -   A generalization of [stepwise regression]{.blue}; -->
  <!--     -   A method based on multi-dimensional [tensor splines]{.orange}; -->
  <!--     -   A modification of [classification and regression trees]{.blue} -->
  <!--         (CART). -->
  
  <!-- -   MARS combine many of the techniques we have seen in this course into a single -->
  <!--     sophisticated algorithm. -->
  <!-- ::: -->
  <!-- ::: -->
  
  <!-- <!-- ```{r} --> -->
  
  <!-- <!-- #| fig-width: 7.8 --> -->
  
  <!-- <!-- #| fig-height: 4 --> -->
  
  <!-- <!-- #| fig-align: center --> -->
  
  <!-- <!-- x <- y <- seq(from = 0, to = 1, length = 50) --> -->
    
    <!-- <!-- xy_seq <- expand.grid(x, y) --> -->
      
      <!-- <!-- z <- matrix(apply(xy_seq, 1, function(x) pmax(0, x[1] - 0.5) * pmax(0, 0.75 - x[2])), nrow = length(x)) --> -->
        
        <!-- <!-- persp(x, y, z, theta = -60, phi = 20, col = "#FF8C00", shade = 0.5, xlab = "x1", ylab = "x2", zlab = "h(x)", ) --> -->
        
        <!-- <!-- ``` --> -->
        
        <!-- ## MARS additive representation -->
        
        <!-- -   MARS is an [additive model]{.blue} of the form: $$ -->
        <!--     f(\bm{x}; \beta) = \beta_0 + \sum_{m=1}^M \beta_m h_m(\bm{x}), -->
          <!--     $$ where $h_m(\bm{x})$ are [basis functions]{.orange} and -->
          <!--     $\beta = (\beta_1,\dots,\beta_M)^T$ are regression coefficients. -->
            
            <!-- . . . -->
            
            <!-- -   Once the basis functions are specified, the estimate for -->
            <!--     $\hat{\beta}$ is straightforward, using for example [least -->
                                                                            <!--     squares]{.blue} or the IWLS algorithm in the classification case. -->
            
            <!-- -   The main distinction with GAMs is that in MARS the basis functions -->
            <!--     are [estimated]{.orange} from the [data]{.orange} and therefore they -->
            <!--     are [not pre-specified]{.orange} in advance. -->
            
            <!-- . . . -->
            
            <!-- -   MARS is essentially a smart [heuristic algorithm]{.blue} for -->
            <!--     selecting a collection of basis functions -->
            <!--     $h_1(\bm{x}),\dots, h_M(\bm{x})$ that hopefully does not incur in -->
            <!--     the [curse of dimensionality]{.orange}. -->
            
            <!-- ## Basis functions for MARS (reflected pairs) -->
            
            <!-- -   The MARS algorithm begins by including just the [intercept -->
                                                                        <!--     term]{.blue}, i.e. $f(\bm{x}; \beta) = \beta_0$. Then, we proceed by -->
                                                                          <!--     [iteratively]{.orange} adding basis functions. -->
                                                                          
                                                                          <!-- . . . -->
                                                                          
                                                                          <!-- -   In MARS the basis functions are always [coupled]{.orange} (or -->
                                                                                                                                                <!--     [reflected]{.orange}), meaning that we always add them in pairs to -->
                                                                          <!--     the additive specification. -->
                                                                          
                                                                          <!-- . . . -->
                                                                          
                                                                          <!-- -   Let us consider the following set of [pairs]{.blue} of [basis -->
                                                                                                                                             <!--     functions]{.blue} (linear splines): $$ -->
                                                                          <!--     \mathcal{C} = \{(x_j - \xi)_+,  (\xi - x_j)_+ : \xi \in \{x_{1j},\dots,x_{nj}\}, \ j=1,\dots,p \}. -->
                                                                            <!--     $$ For example, two basis functions could be -->
                                                                            <!--     $h_1(\bm{x}) = (x_1 - 0.5)_+$ and $h_2(\bm{x}) = (0.5 - x_1)_+$. -->
                                                                              
                                                                              <!-- -   The [knots]{.blue} are placed in correspondence of the [observed data]{.blue}. Hence, -->
                                                                              <!--     there are in [total]{.orange} $2 n p$ [possible basis -->
                                                                                                                                <!--     functions]{.orange} among which we can choose. -->
                                                                              
                                                                              <!-- . . . -->
                                                                              
                                                                              <!-- -   In the [first step]{.blue} of the MARS algorithm, we identify the -->
                                                                              <!--     pair $h_1(\bm{x}) = (x_j - \xi)_+$ and $h_2(\bm{x}) = (\xi - x_j)_+$ -->
                                                                                <!--     that, together with the [intercept]{.blue}, [minimize]{.orange} the -->
                                                                                <!--     [loss function]{.orange}. -->
                                                                                
                                                                                <!-- ## An example of reflected pair basis -->
                                                                                
                                                                                <!-- ```{r} -->
                                                                                <!-- #| fig-width: 7.8 -->
                                                                                <!-- #| fig-height: 3.5 -->
                                                                                <!-- #| fig-align: center -->
                                                                                <!-- x_seq <- seq(from = 0, to = 1, length = 200) -->
                                                                                  <!-- data_plot <- data.frame( -->
                                                                                                                  <!--   x = x_seq, -->
                                                                                                                  <!--   y = c(pmax(0, x_seq - 0.5), pmax(0, 0.5 - x_seq)), -->
                                                                                                                  <!--   basis = rep(c("Basis 1", "Basis 2"), each = length(x_seq)) -->
                                                                                                                  <!-- ) -->
                                                                                    <!-- ggplot(data = data_plot, aes(x = x, y = y, col = basis, linetype = basis)) + -->
                                                                                    <!--   geom_line() + -->
                                                                                    <!--   scale_color_tableau(palette = "Color Blind") + -->
                                                                                    <!--   theme_minimal() + -->
                                                                                    <!--   geom_vline(xintercept = 0.5, linetype = "dotted") + -->
                                                                                    <!--   theme(legend.position = "none") + -->
                                                                                    <!--   xlab("x") + -->
                                                                                    <!--   ylab("Basis function") -->
                                                                                    <!-- ``` -->
                                                                                    
                                                                                    <!-- -   The function $h_1(x) = (x - 0.5)_+$ ([blue]{.blue}) and its -->
                                                                                      <!--     reflection $h_2(x) = (0.5 - x)_+$ ([orange]{.orange}). -->
                                                                                        
                                                                                        <!-- ## A stepwise construction -->
                                                                                        
                                                                                        <!-- ::: incremental -->
                                                                                        <!-- -   Hence, [after]{.orange} the [first step]{.orange} of the MARS -->
                                                                                        <!--     algorithm, our model for example could be $$ -->
                                                                                        <!--     f(\bm{x}; \beta) = \beta_0 + \sum_{m=1}^2\beta_m h_m(\bm{x}) = \beta_0 + \beta_1 (x_1 - 0.5)_+ + \beta_2(0.5 - x_1)_+. -->
                                                                                          <!--     $$ -->
                                                                                          
                                                                                          <!-- -   In the subsequent step, we consider a [new pair]{.blue} of basis -->
                                                                                          <!--     functions $(x_j - \xi)_+, (\xi - x_j)_+$ in $\mathcal{C}$, but this -->
                                                                                          <!--     time we are allowed to perform two kind of operations: -->
                                                                                          
                                                                                          <!--     i.  We can include the new pair to the predictor in an -->
                                                                                          <!--         [additive]{.orange} way, obtaining for example $$ -->
                                                                                          <!--            f(\bm{x}; \beta) =  \beta_0 + \beta_1(x_1 - 0.5)_+ + \beta_2(0.5 - x_1)_+ + \beta_3\textcolor{red}{ (x_2 - 0.75)_+} + \beta_4\textcolor{red}{(0.75 - x_2)_+}. -->
                                                                                            <!--            $$ -->
                                                                                            <!--     ii. We can include the new pair in a [multiplicative]{.orange} way, -->
                                                                                            <!--         by considering the products between the [new basis]{.blue} and -->
                                                                                            <!--         one of the [old bases]{.blue} of the model, obtaining for -->
                                                                                            <!--         instance $$ -->
                                                                                            <!--            \begin{aligned} -->
                                                                                            <!--            f(\bm{x}; \beta) =  \beta_0 &+ \beta_1\textcolor{darkblue}{(x_1 - 0.5)_+} + \beta_2(0.5 - x_1)_+ \\ -->
                                                                                              <!--            &+ \beta_3 \textcolor{red}{(x_1 - 0.5)_+(x_2 - 0.75)_+} + \beta_4 \textcolor{red}{(x_1 - 0.5)_+(0.75 - x_2)_+}. -->
                                                                                              <!--            \end{aligned} -->
                                                                                              <!--            $$ -->
                                                                                              <!-- ::: -->
                                                                                              
                                                                                              <!-- ## An example of tensor product basis -->
                                                                                              
                                                                                              <!-- ![](img/mars.png){width="50%" fig-align="center"} -->
                                                                                              
                                                                                              <!-- -   The [product]{.orange} function -->
                                                                                              <!--     $h(\bm{x}) = (x_1 - 0.5)_+ (x_2 - 0.75)_+$ in the range $(0, 1)^2$. -->
                                                                                                
                                                                                                <!-- ##  -->
                                                                                                
                                                                                                
                                                                                                <!-- ::: callout-note -->
                                                                                                
                                                                                                <!-- #### MARS algorithm (degree $d$) -->
                                                                                                
                                                                                                <!-- ::: incremental -->
                                                                                                
                                                                                                <!-- 1.  Initialize $f(\bm{x}; \beta) = \beta_0$ and let $K$ be [maximum -->
                                                                                                                                                                   <!--     number of pairs]{.orange}, so that $M = 2K$. -->
                                                                                                                                                                     
                                                                                                                                                                     <!-- 2.  Identify the [initial pair]{.blue} of basis functions $h_1$ and -->
                                                                                                                                                                     <!--     $h_2$ in $\mathcal{C}$ that minimize the loss function. Then, -->
                                                                                                                                                                     <!--     let $h_0(\bm{x}) = 1$ and set $\mathcal{M}_1 = \{h_0, h_1, h_2\}$. -->
                                                                                                                                                                       
                                                                                                                                                                       <!-- 3.  For $k = 1,\dots,K - 1$, do: -->
                                                                                                                                                                         
                                                                                                                                                                         <!--     i.  Let $\mathcal{M}_k = \{h_0, h_1,\dots,h_{2k}\}$ be the basis -->
                                                                                                                                                                           <!--         functions already present in the model. -->
                                                                                                                                                                           
                                                                                                                                                                           <!--     ii. Consider a [novel pair]{.blue} of bases -->
                                                                                                                                                                           <!--         $\tilde{h}_1,\tilde{h}_2 \in \mathcal{C} \setminus \mathcal{M}_k$. -->
                                                                                                                                                                           <!--         A [candidate pair]{.orange} is obtained by -->
                                                                                                                                                                           <!--         [multiplying]{.orange} $\tilde{h}_1,\tilde{h}_2$ with one of the -->
                                                                                                                                                                           <!--         bases in $\mathcal{M}_k$. Note that $h_0 \in \mathcal{M}_k$. -->
                                                                                                                                                                           
                                                                                                                                                                           <!--     iii. A [valid]{.orange} candidate basis does not contain the same -->
                                                                                                                                                                           <!--          variable $x_j$ more than once in the product, and it must -->
                                                                                                                                                                           <!--          involve at most [$d$ product terms]{.blue}. -->
                                                                                                                                                                           
                                                                                                                                                                           <!--     iv. Identify the [optimal pair]{.blue} among the candidates at steps -->
                                                                                                                                                                           <!--         (ii)-(iii) that [reduces]{.orange} the [loss function]{.orange} the most. This results in a new pair of bases -->
                                                                                                                                                                           <!--         $h_{2k+1}, h_{2k+2}$. -->
                                                                                                                                                                           
                                                                                                                                                                           <!--     v.  Set -->
                                                                                                                                                                           <!--         $\mathcal{M}_{k+1} \leftarrow \mathcal{M}_k \cup \{h_{2k+1}, h_{2k+2}\}$. -->
                                                                                                                                                                           <!-- 4. Return the collection of models $\mathcal{M}_1,\dots,\mathcal{M}_K$. -->
                                                                                                                                                                           <!-- ::: -->
                                                                                                                                                                           <!-- ::: -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- ## Basis selection and backward regression -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- -   The [degree $d$]{.orange} of the MARS algorithms allow to control -->
                                                                                                                                                                           <!--     the order of interactions of the model. Note that when -->
                                                                                                                                                                           <!--     [$d = 1$]{.blue} it corresponds to a GAM ([no interactions]{.blue}). -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- . . . -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- -   The final model with $M$ terms is very likely [overfitting]{.orange} -->
                                                                                                                                                                           <!--     the data. Hence, it is important to remove some of the bases -->
                                                                                                                                                                           <!--     using [backward regression]{.blue} or best subset. -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- -   The [optimal reduced model]{.orange} can be selected via -->
                                                                                                                                                                           <!--     cross-validation, but generalized cross-validation is often -->
                                                                                                                                                                           <!--     preferred due to computational reasons. -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- . . . -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- -   Unfortunately, it is not clear how to compute the [effective degrees of freedom]{.orange} that are needed in the $\text{GCV}$ formula. -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- -   In MARS, however, we do not have any miraculous simple formula like -->
                                                                                                                                                                           <!--     in LAR or ridge. -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- -   Simulation studies suggest that, for every knot placed, we should -->
                                                                                                                                                                           <!--     pay a [price]{.orange} of about [$3$ degrees of freedom]{.orange}. -->
                                                                                                                                                                           <!--     However, this result is quite [heuristic]{.blue}. -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- ## Heuristics behind MARS -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- -   The basis functions used in MARS have the advantage of [operating -->
                                                                                                                                                                                                                                              <!--     locally]{.orange}. -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- -   When the basis functions in $\mathcal{C}$ are multiplied together, -->
                                                                                                                                                                           <!--     the result is [nonzero]{.orange} only over the small part of the feature space -->
                                                                                                                                                                           <!--     where [both component]{.blue} functions are [nonzero]{.blue}. -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- . . . -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- -   Hence, the estimated function is built up [parsimoniously]{.blue}, -->
                                                                                                                                                                           <!--     by making [small local modifications]{.orange} to the fit obtained at the previous step. -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- -   This is important, since one should "spend" degrees of freedom carefully in high dimensions, to avoid incurring into the [curse of dimensionality]{.orange}. -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- . . . -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- -   The constructional logic of the model is [hierarchical]{.orange}. We -->
                                                                                                                                                                           <!--     can multiply new basis functions that involve new variables only to -->
                                                                                                                                                                           <!--     the basis functions already in the model. -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- -   Hence, an [interaction]{.blue} of a [higher order]{.blue} can only -->
                                                                                                                                                                           <!--     be introduced [when]{.orange} interactions of a [lower order are -->
                                                                                                                                                                                                                                       <!--     present]{.orange}. -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- -   This constraint, introduced for computational reasons, does not -->
                                                                                                                                                                           <!--     necessarily reflect the real behavior of the data, but it often -->
                                                                                                                                                                           <!--     helps in [interpreting the results]{.blue}. -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- ## MARS modeling of `trawl` data ($d = 1$) -->
                                                                                                                                                                           
                                                                                                                                                                           <!-- ```{r} -->
                                                                                                                                                                           <!-- #| message: false -->
                                                                                                                                                                           <!-- #| output: false -->
                                                                                                                                                                           <!-- library(earth) -->
                                                                                                                                                                           <!-- m_mars_deg1 <- earth(Score1 ~ Zone + Year + Latitude + Longitude + Depth, data = trawl_train, degree = 1, pmethod = "exhaustive", penalty = 3, nk = 21) -->
                                                                                                                                                                             <!-- summary(m_mars_deg1, style = "pmax") -->
                                                                                                                                                                             <!-- # plotmo(m_mars_deg1) -->
                                                                                                                                                                             <!-- ``` -->
                                                                                                                                                                             
                                                                                                                                                                             <!-- - We fit a MARS model with $d = 1$ ([no interactions]{.orange}), using $M = 20$. Then, the model was simplified using [best subset selection]{.blue} and $\text{GCV}$. The results are: -->
                                                                                                                                                                               
                                                                                                                                                                               
                                                                                                                                                                               <!-- | Term          | Basis function                    | Coefficient | -->
                                                                                                                                                                               <!-- |---------------|-----------------------------------|-------------| -->
                                                                                                                                                                               <!-- | $h_0(\bm{x})$ | $1$                               | 1.382       | -->
                                                                                                                                                                               <!-- | $h_1(\bm{x})$ | $(\texttt{Longitude} - 143.28)_+$ | -4.275      | -->
                                                                                                                                                                               <!-- | $h_2(\bm{x})$ | $(\texttt{Longitude} - 143.58)_+$ | 3.984       | -->
                                                                                                                                                                               
                                                                                                                                                                               <!-- . . . -->
                                                                                                                                                                               
                                                                                                                                                                               <!-- - To clarify, this specification corresponds to the following [estimated]{.blue} regression [function]{.blue}: -->
                                                                                                                                                                               <!-- $$ -->
                                                                                                                                                                               <!-- f(\bm{x}_i; \hat{\beta}) = 1.382 - 4.275(\texttt{Longitude}_i - 143.28)_+ + 3.984 (\texttt{Longitude}_i - 143.58)_+, -->
                                                                                                                                                                                 <!-- $$ -->
                                                                                                                                                                                 <!-- which has the structure of a GAM. However, the [estimation procedure]{.orange} is [different]{.orange}.  -->
                                                                                                                                                                                 
                                                                                                                                                                                 <!-- . . . -->
                                                                                                                                                                                 
                                                                                                                                                                                 <!-- - The estimated function $f(\bm{x}_i; \hat{\beta})$ is [remarkably simple]{.blue} and it involves only the `Longitude`. Moreover, the relationship between `Score` and `Longitude` is non-linear. -->
                                                                                                                                                                                 
                                                                                                                                                                                 <!-- - Both these considerations are consistent with the previous findings, obtained using GAMs.  -->
                                                                                                                                                                                 
                                                                                                                                                                                 <!-- ## MARS modeling of `trawl` data ($d = 2$) -->
                                                                                                                                                                                 
                                                                                                                                                                                 <!-- - We fit a MARS model with $d = 2$ ([first order interactions]{.orange}), using $M = 20$. As before, the model was simplified using [best subset selection]{.blue} and $\text{GCV}$. The results are: -->
                                                                                                                                                                                   
                                                                                                                                                                                   
                                                                                                                                                                                   <!-- ```{r} -->
                                                                                                                                                                                   <!-- #| output: false -->
                                                                                                                                                                                   <!-- m_mars_deg2 <- earth(Score1 ~ Zone + Year + Latitude + Longitude + Depth, -->
                                                                                                                                                                                                               <!--   data = trawl_train, degree = 2, -->
                                                                                                                                                                                                               <!--   pmethod = "exhaustive", penalty = 4, trace = TRUE, nk = 21 -->
                                                                                                                                                                                                               <!-- ) -->
                                                                                                                                                                                     <!-- summary(m_mars_deg2) -->
                                                                                                                                                                                     <!-- # plotmo(m_mars_deg2) -->
                                                                                                                                                                                     <!-- ``` -->
                                                                                                                                                                                     
                                                                                                                                                                                     
                                                                                                                                                                                     <!-- | Term          | Basis function                                                     | Coefficient | -->
                                                                                                                                                                                     <!-- |----------|----------------------------------------------|-------------------------| -->
                                                                                                                                                                                     <!-- | $h_0(\bm{x})$ | $1$                                                                | 1.318       | -->
                                                                                                                                                                                     <!-- | $h_1(\bm{x})$ | $(\texttt{Longitude} - 143.28)_+$                                  | -5.388      | -->
                                                                                                                                                                                     <!-- | $h_2(\bm{x})$ | $(\texttt{Longitude} - 143.58)_+$                                  | 4.172       | -->
                                                                                                                                                                                     <!-- | $h_3(\bm{x})$ | $I(\texttt{Year} = \texttt{1993})(\texttt{Longitude} - 143.05)_+$           | 0.679       | -->
                                                                                                                                                                                     <!-- | $h_4(\bm{x})$ | $[\texttt{Latitude} - (-11.72)]_+ (\texttt{Longitude} - 143.05)_+$ | 1.489       | -->
                                                                                                                                                                                     
                                                                                                                                                                                     <!-- . . . -->
                                                                                                                                                                                     
                                                                                                                                                                                     <!-- - As expected, a degree $2$ MARS lead to a more sophisticated fit involving [interactions]{.blue} between `Year` and `Longitude` as well as between `Latitude` and `Longitude`. -->
                                                                                                                                                                                     
                                                                                                                                                                                     <!-- - We can explore these effects using [partial plots]{.orange}.  -->
                                                                                                                                                                                     
                                                                                                                                                                                     <!-- ## Partial effects -->
                                                                                                                                                                                     
                                                                                                                                                                                     <!-- ```{r} -->
                                                                                                                                                                                     <!-- library(pdp) -->
                                                                                                                                                                                     <!-- partial_linear <- partial(m_linear, pred.var = c("Longitude", "Year"), grid.resolution = 40) -->
                                                                                                                                                                                       <!-- partial_gam <- partial(m_gam, pred.var = c("Longitude", "Year"), grid.resolution = 40) -->
                                                                                                                                                                                         <!-- partial_mars_deg1 <- partial(m_mars_deg1, pred.var = c("Longitude", "Year"), grid.resolution = 40) -->
                                                                                                                                                                                           <!-- partial_mars_deg2 <- partial(m_mars_deg2, pred.var = c("Longitude", "Year"), grid.resolution = 40) -->
                                                                                                                                                                                             <!-- ``` -->
                                                                                                                                                                                             
                                                                                                                                                                                             <!-- ::: panel-tabset -->
                                                                                                                                                                                             <!-- ## Linear model -->
                                                                                                                                                                                             
                                                                                                                                                                                             <!-- ```{r} -->
                                                                                                                                                                                             <!-- #| fig-width: 9 -->
                                                                                                                                                                                             <!-- #| fig-height: 4.5 -->
                                                                                                                                                                                             <!-- #| fig-align: center -->
                                                                                                                                                                                             <!-- #| message: false -->
                                                                                                                                                                                             <!-- ggplot(data = trawl_train, aes(x = Longitude, y = Score1, col = Year)) + -->
                                                                                                                                                                                             <!--   geom_point(size = 1) + -->
                                                                                                                                                                                             <!--   facet_grid(. ~ Year) + -->
                                                                                                                                                                                             <!--   geom_line(data = partial_linear, aes(x = Longitude, y = yhat)) + -->
                                                                                                                                                                                             <!--   scale_color_tableau(palette = "Color Blind") + -->
                                                                                                                                                                                             <!--   theme_light() + -->
                                                                                                                                                                                             <!--   theme(legend.position = "none") + -->
                                                                                                                                                                                             <!--   xlab("Longitude of the sampling position") + -->
                                                                                                                                                                                             <!--   ylab("Catch score") -->
                                                                                                                                                                                             <!-- ``` -->
                                                                                                                                                                                             
                                                                                                                                                                                             <!-- ## GAM model -->
                                                                                                                                                                                             
                                                                                                                                                                                             <!-- ```{r} -->
                                                                                                                                                                                             <!-- #| fig-width: 9 -->
                                                                                                                                                                                             <!-- #| fig-height: 4.5 -->
                                                                                                                                                                                             <!-- #| fig-align: center -->
                                                                                                                                                                                             <!-- #| message: false -->
                                                                                                                                                                                             <!-- ggplot(data = trawl_train, aes(x = Longitude, y = Score1, col = Year)) + -->
                                                                                                                                                                                             <!--   geom_point(size = 1) + -->
                                                                                                                                                                                             <!--   facet_grid(. ~ Year) + -->
                                                                                                                                                                                             <!--   geom_line(data = partial_gam, aes(x = Longitude, y = yhat)) + -->
                                                                                                                                                                                             <!--   scale_color_tableau(palette = "Color Blind") + -->
                                                                                                                                                                                             <!--   theme_light() + -->
                                                                                                                                                                                             <!--   theme(legend.position = "none") + -->
                                                                                                                                                                                             <!--   xlab("Longitude of the sampling position") + -->
                                                                                                                                                                                             <!--   ylab("Catch score") -->
                                                                                                                                                                                             <!-- ``` -->
                                                                                                                                                                                             
                                                                                                                                                                                             <!-- ## MARS (degree 1) -->
                                                                                                                                                                                             
                                                                                                                                                                                             <!-- ```{r} -->
                                                                                                                                                                                             <!-- #| fig-width: 9 -->
                                                                                                                                                                                             <!-- #| fig-height: 4.5 -->
                                                                                                                                                                                             <!-- #| fig-align: center -->
                                                                                                                                                                                             <!-- #| message: false -->
                                                                                                                                                                                             <!-- ggplot(data = trawl_train, aes(x = Longitude, y = Score1, col = Year)) + -->
                                                                                                                                                                                             <!--   geom_point(size = 1) + -->
                                                                                                                                                                                             <!--   facet_grid(. ~ Year) + -->
                                                                                                                                                                                             <!--   geom_line(data = partial_mars_deg1, aes(x = Longitude, y = yhat)) + -->
                                                                                                                                                                                             <!--   scale_color_tableau(palette = "Color Blind") + -->
                                                                                                                                                                                             <!--   theme_light() + -->
                                                                                                                                                                                             <!--   theme(legend.position = "none") + -->
                                                                                                                                                                                             <!--   xlab("Longitude of the sampling position") + -->
                                                                                                                                                                                             <!--   ylab("Catch score") -->
                                                                                                                                                                                             <!-- ``` -->
                                                                                                                                                                                             
                                                                                                                                                                                             <!-- ## MARS (degree 2) -->
                                                                                                                                                                                             
                                                                                                                                                                                             <!-- ```{r} -->
                                                                                                                                                                                             <!-- #| fig-width: 9 -->
                                                                                                                                                                                             <!-- #| fig-height: 4.5 -->
                                                                                                                                                                                             <!-- #| fig-align: center -->
                                                                                                                                                                                             <!-- #| message: false -->
                                                                                                                                                                                             <!-- ggplot(data = trawl_train, aes(x = Longitude, y = Score1, col = Year)) + -->
                                                                                                                                                                                             <!--   geom_point(size = 1) + -->
                                                                                                                                                                                             <!--   facet_grid(. ~ Year) + -->
                                                                                                                                                                                             <!--   geom_line(data = partial_mars_deg2, aes(x = Longitude, y = yhat)) + -->
                                                                                                                                                                                             <!--   scale_color_tableau(palette = "Color Blind") + -->
                                                                                                                                                                                             <!--   theme_light() + -->
                                                                                                                                                                                             <!--   theme(legend.position = "none") + -->
                                                                                                                                                                                             <!--   xlab("Longitude of the sampling position") + -->
                                                                                                                                                                                             <!--   ylab("Catch score") -->
                                                                                                                                                                                             <!-- ``` -->
                                                                                                                                                                                             <!-- ::: -->
                                                                                                                                                                                             
                                                                                                                                                                                             