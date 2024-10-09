#' Image of the data structure
#'
#' @description
#' `extract_structure` generates an image of the multilevel data structure. It
#' does this in two steps. First, characteristics of the group(s) or grouping
#' factor(s) are identified via the `model` input or the `n_gf`,
#' `gf_description`, and `gf_names` inputs. Second, this information is used to
#' run [DiagrammeR::grViz()], which returns an image.
#'
#' @param model Code for fitting a [nlme::lme()] or [lme4::lmer()] model given
#'  as a string.
#' @param n_gf Number of groups or grouping factors: `1`, `2`, or `3`. Only
#'  applies if `model` is `NULL`.
#' @param gf_description Description of the structure of the groups or grouping
#'  factors: `"nested"`, `"crossed"`, `"crossed with nested"`, or
#'  `"crossed within nested"`. Only applies if `n_gf` is greater than `1` and
#'  `model` is `NULL`.
#' @param gf_names Character vector of the names of group(s) or grouping
#'  factor(s). For nested, order names by level from highest to lowest. Must be
#'  a vector of length equal to `n_gf`. Only applies if `model` is `NULL`.
#' @param gf_nlevels Optional numeric or character vector of the number of
#'  levels for each group or grouping factor in the `model` or `gf_names`.
#' @param gf3_index String for the index of the highest-level group or grouping
#'  factor. Only applies if `n_gf` is `3`. Default is `"i"`.
#' @param label_levels Indicates whether levels of the data structure should be
#'  labeled on the left-hand side of the figure (default) or not
#'  (`label_levels` = `"no"`).
#' @param export_type Export type can be `"print"` (default), `"png"` to save
#'  as a PNG file, or `"text"` to get the input used to run
#'  [DiagrammeR::grViz()].
#'
#' @return A PNG (`export_type = "png"`), character
#'  (`export_type = "text"`), or object of class htmlwidget that will print
#'  in the R console, within R Markdown documents, and within Shiny output
#'  bindings (`export_type = "print"`).
#' @export
#'
#' @examples
#' # Using the model input
#' extract_structure(model = "lme(Score ~ type, random=list(School=pdDiag(~1+type),Class=~1))")
#' extract_structure(model = "lme(Weight ~ Time, random=~Time|Subject, data)",
#'                   gf_nlevels = 47)
#' extract_structure(model = "lmer(Strength ~ 1 + (1|Machine) + (1|Worker))",
#'                   gf_nlevels = c("23", "J"))
#'
#' # Using the n_gf, gf_description, and gf_names inputs
#' extract_structure(n_gf = 1,
#'                   gf_names = "Subject")
#' extract_structure(n_gf = 3,
#'                   gf_description = "nested",
#'                   gf_names = c("District", "School", "Class"),
#'                   gf_nlevels = c(8, 15, 5),
#'                   label_levels = "no")
extract_structure <- function(model = NULL,
                              n_gf = NULL,
                              gf_description = NULL,
                              gf_names = NULL,
                              gf_nlevels = NULL,
                              gf3_index  =  "i",
                              label_levels = "yes",
                              export_type = "print") {

  if (is.null(model)) {
    if (is.null(n_gf)) {
      cli::cli_abort(c(
        "Can't find input 'n_gf' or 'model'.",
        "i" = "The required inputs are 'model' or 'n_gf', 'gf_description', and 'gf_names'."
      ))
    }
    if (!n_gf %in% c(1, 2, 3)) {
      cli::cli_abort(c(
        "'n_gf' must be 1, 2, or 3."
      ))
    }
    if (is.null(gf_names)) {
      cli::cli_abort(c(
        "Can't find input 'gf_names' or 'model'.",
        "i" = "The required inputs are 'model' or 'n_gf', 'gf_description', and 'gf_names'."
      ))
    }
    if (n_gf != length(gf_names)) {
      cli::cli_abort(c(
        "'gf_names' must have length equal to 'n_gf'."
      ))
    }
    if (n_gf == 1) {
      gf_description <- ""
    }
    if (is.null(gf_description)) {
      cli::cli_abort(c(
        "Can't find input 'gf_description' or 'model'.",
        "i" = "The required inputs are 'model' or 'n_gf', 'gf_description', and 'gf_names'."
      ))
    }
    if (!gf_description %in% c("nested", "crossed", "", "crossed with nested", "crossed within nested")) {
      cli::cli_abort(c(
        "Check 'gf_description'."
      ))
    }
    gf_nest <- c("", "", "")
    gf_cross <- c("", "", "")
    if (gf_description == "") {
      gf_cross <- gf_names
    }
    if (gf_description == "crossed") {
      gf_cross <- gf_names
    }
    if (gf_description == "nested") {
      gf_nest <- gf_names
    }
    if (gf_description == "crossed with nested") {
      gf_cross[1] <- gf_names[1]
      gf_nest[1:2] <- gf_names[2:3]
    }
    if (gf_description == "crossed within nested") {
      gf_cross[1:2] <- gf_names[2:3]
      gf_nest[1] <- gf_names[1]
    }
  } else {
    extract_gf_output <- extract_gf(model = model)
    n_gf <- extract_gf_output[[4]]
    gf_description <- extract_gf_output[[5]]
    gf_cross <- unlist(extract_gf_output[[6]])
    gf_nest <- unlist(extract_gf_output[[7]])
    gf_names <- c("", "", "")
    if (gf_description == "") {
      gf_cross -> gf_names
    }
    if (gf_description == "crossed") {
      gf_cross -> gf_names
    }
    if (gf_description == "nested") {
      gf_nest -> gf_names
    }
    if (gf_description == "crossed with nested") {
      gf_cross[1] -> gf_names[1]
      gf_nest[1:2] -> gf_names[2:3]
    }
    if (gf_description == "crossed within nested") {
      gf_cross[1:2] -> gf_names[2:3]
      gf_nest[1] -> gf_names[1]
    }
    gf_names <- gf_names[which(gf_names != "")]
  }

  use_gf_nlevels <- "yes"
  gf_nlevelsA <- rep(0, n_gf)
  gf_nlevelsB <- rep("", n_gf)
  if (is.null(gf_nlevels)) {
    use_gf_nlevels <- "no"
  } else {
    if (n_gf != length(gf_nlevels)) {
      cli::cli_abort(c(
        "'gf_nlevels' must have length equal to 'n_gf'."
      ))
    }
    for (i in 1:length(gf_nlevels)) {
      check_gf_nlevels <- suppressWarnings(as.numeric(gf_nlevels[i]))
      if (!is.na(check_gf_nlevels)) {
        gf_nlevelsA[i] <- check_gf_nlevels
        if (check_gf_nlevels != round(check_gf_nlevels) || check_gf_nlevels <= 2) {
          cli::cli_abort(c(
            "'gf_nlevels' must be greater than or equal to 3 if numeric."
          ))
        }
      } else {
        gf_nlevelsB[i] <- gf_nlevels[i]
      }
    }
  }

  if (n_gf == 1) {
    diagram_text <- "\n  digraph {\n    graph [rankdir = TB]\n    edge [color=black]\n    node[shape=box]\n    label1[label = \"Level 2\", color=white]\n    label2[label = \"Level 1\", color=white]\n    B[label = \"ID 1\"]\n    C[label = \"ID 2\"]\n    Y[label = \"ID 3\"]\n    D[label = \". . .\", color=white]\n    E[label = \"ID a\"]\n    node[width = 0.5, fontsize=10]\n    F[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    G[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    Z[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    H[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    label3[label = \"\", color=white]\n    label4[label = \"\", color=white]\n    B->F\n    C->G\n    Y->Z\n    E->H\n    edge [color=white]\n    label1->label2\n    label3->label4\n    edge [style = dashed,color=black,minlen=1,arrowhead=none]\n    rank=same {label1->B}\n    rank=same {E->label3}\n    edge [color=gray75,style = dashed,minlen=1,arrowhead=none]\n    rank=same {label2->F}\n    rank=same {H->label4}\n  }\n   "
    if (use_gf_nlevels == "yes") {
      if (gf_nlevelsA >= 5 || gf_nlevelsB != "") {
        r1 <- paste("ID", gf_nlevels)
        diagram_text <- stringr::str_replace(diagram_text, stringr::fixed("ID a"), r1)
      } else {
        diagram_text <- "\n  digraph {\n    graph [rankdir = TB]\n    edge [color=black]\n    node[shape=box]\n    label1[label = \"Level 2\", color=white]\n    label2[label = \"Level 1\", color=white]\n    B[label = \"ID 1\"]\n    C[label = \"ID 2\"]\n    Y[label = \"ID 3\"]\n    E[label = \"ID 4\"]\n    node[width = 0.5, fontsize=10]\n    F[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    G[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    Z[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    H[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    label3[label = \"\", color=white]\n    label4[label = \"\", color=white]\n    B->F\n    C->G\n    Y->Z\n    E->H\n    edge [color=white]\n    label1->label2\n    label3->label4\n    edge [style = dashed,color=black,minlen=1,arrowhead=none]\n    rank=same {label1->B}\n    rank=same {E->label3}\n    edge [color=gray75,style = dashed,minlen=1,arrowhead=none]\n    rank=same {label2->F}\n    rank=same {H->label4}\n  }\n   "
        if (gf_nlevelsA == 3) {
          diagram_text <- "\n  digraph {\n    graph [rankdir = TB]\n    edge [color=black]\n    node[shape=box]\n    label1[label = \"Level 2\", color=white]\n    label2[label = \"Level 1\", color=white]\n    B[label = \"ID 1\"]\n    C[label = \"ID 2\"]\n    Y[label = \"ID 3\"]\n    node[width = 0.5, fontsize=10]\n    F[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    G[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    Z[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    label3[label = \"\", color=white]\n    label4[label = \"\", color=white]\n    B->F\n    C->G\n    Y->Z\n    edge [color=white]\n    label1->label2\n    label3->label4\n    edge [style = dashed,color=black,minlen=1,arrowhead=none]\n    rank=same {label1->B}\n    rank=same {Y->label3}\n    edge [color=gray75,style = dashed,minlen=1,arrowhead=none]\n    rank=same {label2->F}\n    rank=same {Z->label4}\n  }\n   "
        }
      }
    }
    diagram_text <- stringr::str_replace_all(diagram_text, "ID", gf_cross[1])

  }else if (n_gf == 2 && gf_description == "nested") {
    diagram_text <- "\n  digraph {\n    graph [layout = dot,rankdir = TB]\n    edge [color=black]\n    node [shape = box]\n    label1[label = \"Level 3\", color=white]\n    label2[label = \"Level 2\", color=white]\n    label3[label = \"Level 1\", color=white]\n    A[label = \"lvl3GF\n 1\",shape=oval]\n    B[label = \"lvl2GF\n 1(1)\"]\n    C[label = \"lvl2GF\n 2(1)\"]\n    D[label = \". . .\", color=white]\n    E[label = \"lvl2GF\n b(1)\"]\n    F[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    G[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    H[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    I[label = \"lvl3GF\n 2\",shape=oval]\n    J[label = \"lvl2GF\n 1(2)\"]\n    K[label = \"lvl2GF\n 2(2)\"]\n    L[label = \". . .\", color=white]\n    M[label = \"lvl2GF\n b(2)\"]\n    N[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    O[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    P[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    R[label = \". . .\",color=white]\n    S[label = \" \",color=white]\n    Q[label = \"lvl3GF\n a\",shape=oval]\n    T[label = \"lvl2GF\n 1(a)\"]\n    U[label = \"lvl2GF\n 2(a)\"]\n    V[label = \". . .\", color=white]\n    W[label = \"lvl2GF\n b(a)\"]\n    X[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    Y[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    Z[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    label4[label = \"\", color=white]\n    label5[label = \"\", color=white]\n    label6[label = \"\", color=white]\n    A->B\n    A->C\n    A->E\n    B->F\n    C->G\n    E->H\n    I->J\n    I->K\n    I->M\n    J->N\n    K->O\n    M->P\n    Q->T\n    Q->U\n    Q->W\n    T->X\n    U->Y\n    W->Z\n    edge [color=white]\n    rank=same{A->I->R->Q}\n    A->D\n    I->L\n    R->S\n    Q->V\n    label1->label2->label3\n    label4->label5->label6\n    edge [minlen = 9,color=black,style = dashed,arrowhead=none]\n    rank=same {label1->A}\n    rank=same {Q->label4}\n    edge [minlen = 1,color=black,style = dashed,arrowhead=none]\n    rank=same {label2->B}\n    rank=same {W->label5}\n    edge [minlen = 1,color=gray75,style = dashed,arrowhead=none]\n    rank=same {label3->F}\n    rank=same {Z->label6}\n  }\n   "
    if (use_gf_nlevels == "yes") {
      if (gf_nlevelsA[1] == 3) {
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("R[label = \". . .\",color=white]\n    S[label = \" \",color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("R->S\n    "))
        diagram_text <- stringr::str_replace(diagram_text,
                                             stringr::fixed("rank=same{A->I->R->Q}"),
                                             stringr::fixed("rank=same{A->I->Q}"))
      }
      if (gf_nlevelsA[2] == 3) {
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("D[label = \". . .\", color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("L[label = \". . .\", color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("V[label = \". . .\", color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("A->D\n    I->L\n    "))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("Q->V\n    "))
      }
      if (gf_nlevelsA[2] >= 3 || gf_nlevelsB[2] != "") {
        r1 <- paste("lvl2GF\n", gf_nlevels[2])
        diagram_text <- stringr::str_replace_all(diagram_text, stringr::fixed("lvl2GF\n b"), r1)
      }
      if (gf_nlevelsA[1] >= 3 || gf_nlevelsB[1] != "") {
        r1 <- paste("lvl3GF\n", gf_nlevels[1])
        diagram_text <- stringr::str_replace_all(diagram_text, stringr::fixed("lvl3GF\n a"), r1)
        r3 <- paste0("(", gf_nlevels[1], ")")
        diagram_text <- stringr::str_replace_all(diagram_text, stringr::fixed("(a)"), r3)
      }
    }
    diagram_text <- stringr::str_replace_all(diagram_text, "lvl3GF", gf_nest[1])
    diagram_text <- stringr::str_replace_all(diagram_text, "lvl2GF", gf_nest[2])

  }else if (n_gf == 3 && gf_description == "nested") {
    diagram_text <- "\n  digraph {\n    graph [layout = dot,rankdir = TB]\n    edge [color=black]\n    node [shape = box]\n    label0[label = \"Level 4\", color=white]\n    label1[label = \"Level 3\", color=white]\n    label2[label = \"Level 2\", color=white]\n    label3[label = \"Level 1\", color=white]\n    AA[label = \"lvl4GF\n i=1,...,a\", shape=diamond]\n    A[label = \"lvl3GF\n 1(i)\",shape=oval]\n    B[label = \"lvl2GF\n 1(i1)\"]\n    C[label = \"lvl2GF\n 2(i1)\"]\n    D[label = \". . .\", color=white]\n    E[label = \"lvl2GF\n c(i1)\"]\n    F[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    G[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    H[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    I[label = \"lvl3GF\n 2(i)\",shape=oval]\n    J[label = \"lvl2GF\n 1(i2)\"]\n    K[label = \"lvl2GF\n 2(i2)\"]\n    L[label = \". . .\", color=white]\n    M[label = \"lvl2GF\n c(i2)\"]\n    N[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    O[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    P[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    R[label = \". . .\",color=white]\n    S[label = \" \",color=white]\n    Q[label = \"lvl3GF\n b(i)\",shape=oval]\n    T[label = \"lvl2GF\n 1(ib)\"]\n    U[label = \"lvl2GF\n 2(ib)\"]\n    V[label = \". . .\", color=white]\n    W[label = \"lvl2GF\n c(ib)\"]\n    X[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    Y[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    Z[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    label35[label = \"\", color=white]\n    label4[label = \"\", color=white]\n    label5[label = \"\", color=white]\n    label6[label = \"\", color=white]\n    AA->A\n    AA->I\n    AA->Q\n    A->B\n    A->C\n    A->E\n    B->F\n    C->G\n    E->H\n    I->J\n    I->K\n    I->M\n    J->N\n    K->O\n    M->P\n    Q->T\n    Q->U\n    Q->W\n    T->X\n    U->Y\n    W->Z\n    edge [color=white]\n    AA->R\n    rank=same{A->I->R->Q}\n    A->D\n    I->L\n    R->S\n    Q->V\n    label0->label1->label2->label3\n    label35->label4->label5->label6\n    edge [minlen = 23,color=black,style = dashed,arrowhead=none]\n    rank=same {label0->AA}\n    rank=same {AA->label35}\n    edge [minlen = 12,color=black,style = dashed,arrowhead=none]\n    rank=same {label1->A}\n    rank=same {Q->label4}\n    edge [minlen = 1,color=black,style = dashed,arrowhead=none]\n    rank=same {label2->B}\n    rank=same {W->label5}\n    edge [minlen = 1,color=gray75,style = dashed,arrowhead=none]\n    rank=same {label3->F}\n    rank=same {Z->label6}\n  }\n   "
    if (use_gf_nlevels == "yes") {
      if (gf_nlevelsA[2] == 3) {
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("R[label = \". . .\",color=white]\n    S[label = \" \",color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("R->S\n    "))
        diagram_text <- stringr::str_replace(diagram_text,
                                             stringr::fixed("rank=same{A->I->R->Q}"),
                                             stringr::fixed("rank=same{A->I->Q}"))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("AA->R\n    "))
      }
      if (gf_nlevelsA[3] == 3) {
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("D[label = \". . .\", color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("L[label = \". . .\", color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("V[label = \". . .\", color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("A->D\n    I->L\n    "))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("Q->V\n    "))
      }
      if (gf_nlevelsA[2] == 3 && gf_nlevelsA[3] == 3) {
        diagram_text <- stringr::str_replace(diagram_text,
                                             stringr::fixed("minlen = 23"),
                                             stringr::fixed("minlen = 15"))
        diagram_text <- stringr::str_replace(diagram_text,
                                             stringr::fixed("minlen = 12"),
                                             stringr::fixed("minlen = 10"))
      }else if (gf_nlevelsA[2] == 3 && gf_nlevelsA[3] != 3) {
        diagram_text <- stringr::str_replace(diagram_text,
                                             stringr::fixed("minlen = 23"),
                                             stringr::fixed("minlen = 19"))
      } else if(gf_nlevelsA[2] != 3 && gf_nlevelsA[3] == 3) {
        diagram_text <- stringr::str_replace(diagram_text,
                                             stringr::fixed("minlen = 23"),
                                             stringr::fixed("minlen = 17"))
        diagram_text <- stringr::str_replace(diagram_text,
                                             stringr::fixed("minlen = 12"),
                                             stringr::fixed("minlen = 10"))
      }
      if (gf_nlevelsA[3] >= 3 || gf_nlevelsB[3] != "") {
        r3 <- paste("lvl2GF\n", gf_nlevels[3])
        diagram_text <- stringr::str_replace_all(diagram_text, stringr::fixed("lvl2GF\n c"), r3)
      }
      if (gf_nlevelsA[2] >= 3 || gf_nlevelsB[2] != "") {
        r2a <- paste("lvl3GF\n", gf_nlevels[2])
        diagram_text <- stringr::str_replace_all(diagram_text, stringr::fixed("lvl3GF\n b"), r2a)
        r2b <- paste0("(i", gf_nlevels[2], ")")
        diagram_text <- stringr::str_replace_all(diagram_text, stringr::fixed("(ib)"), r2b)
      }
      r1 <- paste0("lvl4GF\n i=1,...,", gf_nlevels[1])
      diagram_text <- stringr::str_replace(diagram_text, stringr::fixed("lvl4GF\n i=1,...,a"), r1)
    }
    if (gf3_index != "i") {
      r1a <- paste("lvl4GF\n", gf3_index)
      diagram_text <- stringr::str_replace(diagram_text, stringr::fixed("lvl4GF\n i"), r1a)
      r1b <- paste0("(", gf3_index)
      diagram_text <- stringr::str_replace_all(diagram_text, stringr::fixed("(i"), r1b)
    }
    diagram_text <- stringr::str_replace_all(diagram_text, "lvl4GF", gf_nest[1])
    diagram_text <- stringr::str_replace_all(diagram_text, "lvl3GF", gf_nest[2])
    diagram_text <- stringr::str_replace_all(diagram_text, "lvl2GF", gf_nest[3])

  }else if (n_gf == 2 && gf_description == "crossed") {
    diagram_text <- "\n  digraph {\n    graph [layout = dot,rankdir = TB]\n    edge [color=black]\n    node [shape = box]\n    label1[label = \"Level 2\", color=white]\n    label2[label = \"\", color=white]\n    label3[label = \"Level 1\", color=white]\n    A[label = \"lvl2aGF\n 1\"]\n    B[label = \"lvl2bGF\n 1\"]\n    C[label = \"lvl2bGF\n 2\"]\n    D[label = \". . .\", color=white]\n    E[label = \"lvl2bGF\n b\"]\n    F[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    G[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    H[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    I[label = \"lvl2aGF\n 2\"]\n    J[label = \"lvl2bGF\n 1\"]\n    K[label = \"lvl2bGF\n 2\"]\n    L[label = \". . .\", color=white]\n    M[label = \"lvl2bGF\n b\"]\n    N[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    O[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    P[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    R[label = \". . .\",color=white]\n    S[label = \" \",color=white]\n    Q[label = \"lvl2aGF\n a\"]\n    T[label = \"lvl2bGF\n 1\"]\n    U[label = \"lvl2bGF\n 2\"]\n    V[label = \". . .\", color=white]\n    W[label = \"lvl2bGF\n b\"]\n    X[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    Y[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    Z[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    label4[label = \"\", color=white]\n    label5[label = \"\", color=white]\n    label6[label = \"\", color=white]\n    edge[arrowhead = none]\n    A->B\n    A->C\n    A->E\n    edge[arrowhead = normal]\n    B->F\n    C->G\n    E->H\n    edge[arrowhead = none]\n    I->J\n    I->K\n    I->M\n    edge[arrowhead = normal]\n    J->N\n    K->O\n    M->P\n    edge[arrowhead = none]\n    Q->T\n    Q->U\n    Q->W\n    edge[arrowhead = normal]\n    T->X\n    U->Y\n    W->Z\n    edge [color=white]\n    rank=same{A->I->R->Q}\n    A->D\n    I->L\n    R->S\n    Q->V\n    label1->label2->label3\n    label4->label5->label6\n    edge [minlen = 9,color=black,style = dashed,arrowhead=none]\n    rank=same {label1->A}\n    rank=same {Q->label4}\n    edge [minlen = 2,color=black,style = dashed,arrowhead=none]\n    rank=same {label2->B}\n    rank=same {W->label5}\n    edge [minlen = 1,color=gray75,style = dashed,arrowhead=none]\n    rank=same {label3->F}\n    rank=same {Z->label6}\n  }\n   "
    if (use_gf_nlevels == "yes") {
      if ((gf_nlevelsA[1] >= 3 || gf_nlevelsB[1] != "") && gf_nlevelsA[2] == 2) {
        diagram_text <- "\n  digraph {\n    graph [layout = dot,rankdir = TB]\n    edge [color=black]\n    node [shape = box]\n    label1[label = \"Level 2\", color=white]\n    label2[label = \"\", color=white]\n    label3[label = \"Level 1\", color=white]\n    A[label = \"lvl2aGF\n 1\"]\n    B[label = \"lvl2bGF\n 1\"]\n    C[label = \"lvl2bGF\n 2\"]\n    F[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    G[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    I[label = \"lvl2aGF\n 2\"]\n    J[label = \"lvl2bGF\n 1\"]\n    K[label = \"lvl2bGF\n 2\"]\n    N[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    O[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    R[label = \". . .\",color=white]\n    S[label = \" \",color=white]\n    Q[label = \"lvl2aGF\n a\"]\n    T[label = \"lvl2bGF\n 1\"]\n    U[label = \"lvl2bGF\n 2\"]\n    X[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    Y[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    label4[label = \"\", color=white]\n    label5[label = \"\", color=white]\n    label6[label = \"\", color=white]\n    edge[arrowhead = none]\n    A->B\n    A->C\n    edge[arrowhead = normal]\n    B->F\n    C->G\n    edge[arrowhead = none]\n    I->J\n    I->K\n    edge[arrowhead = normal]\n    J->N\n    K->O\n    edge[arrowhead = none]\n    Q->T\n    Q->U\n    edge[arrowhead = normal]\n    T->X\n    U->Y\n    edge [color=white]\n    rank=same{A->I->R->Q}\n    R->S\n    label1->label2->label3\n    label4->label5->label6\n    edge [minlen = 9,color=black,style = dashed,arrowhead=none]\n    rank=same {label1->A}\n    rank=same {Q->label4}\n    edge [minlen = 2,color=black,style = dashed,arrowhead=none]\n    rank=same {label2->B}\n    rank=same {U->label5}\n    edge [minlen = 1,color=gray75,style = dashed,arrowhead=none]\n    rank=same {label3->F}\n    rank=same {Y->label6}\n  }\n   "
      }else if (gf_nlevelsA[1] == 2 && (gf_nlevelsA[2] >= 3 || gf_nlevelsB[2] != "")) {
        diagram_text <- "\n  digraph {\n    graph [layout = dot,rankdir = TB]\n    edge [color=black]\n    node [shape = box]\n    label1[label = \"Level 2\", color=white]\n    label2[label = \"\", color=white]\n    label3[label = \"Level 1\", color=white]\n    A[label = \"lvl2aGF\n 1\"]\n    B[label = \"lvl2bGF\n 1\"]\n    C[label = \"lvl2bGF\n 2\"]\n    D[label = \". . .\", color=white]\n    E[label = \"lvl2bGF\n b\"]\n    F[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    G[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    H[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    I[label = \"lvl2aGF\n 2\"]\n    J[label = \"lvl2bGF\n 1\"]\n    K[label = \"lvl2bGF\n 2\"]\n    L[label = \". . .\", color=white]\n    M[label = \"lvl2bGF\n b\"]\n    N[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    O[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    P[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    label4[label = \"\", color=white]\n    label5[label = \"\", color=white]\n    label6[label = \"\", color=white]\n    edge[arrowhead = none]\n    A->B\n    A->C\n    A->E\n    edge[arrowhead = normal]\n    B->F\n    C->G\n    E->H\n    edge[arrowhead = none]\n    I->J\n    I->K\n    I->M\n    edge[arrowhead = normal]\n    J->N\n    K->O\n    M->P\n    edge [color=white]\n    rank=same{A->I}\n    A->D\n    I->L\n    label1->label2->label3\n    label4->label5->label6\n    edge [minlen = 9,color=black,style = dashed,arrowhead=none]\n    rank=same {label1->A}\n    rank=same {I->label4}\n    edge [minlen = 2,color=black,style = dashed,arrowhead=none]\n    rank=same {label2->B}\n    rank=same {M->label5}\n    edge [minlen = 1,color=gray75,style = dashed,arrowhead=none]\n    rank=same {label3->F}\n    rank=same {P->label6}\n  }\n   "
      }else if (gf_nlevelsA[1] == 2 && gf_nlevelsA[2] == 2) {
        diagram_text <- "\n  digraph {\n    graph [layout = dot,rankdir = TB]\n    edge [color=black]\n    node [shape = box]\n    label1[label = \"Level 2\", color=white]\n    label2[label = \"\", color=white]\n    label3[label = \"Level 1\", color=white]\n    A[label = \"lvl2aGF\n 1\"]\n    B[label = \"lvl2bGF\n 1\"]\n    C[label = \"lvl2bGF\n 2\"]\n    F[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    G[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    I[label = \"lvl2aGF\n 2\"]\n    J[label = \"lvl2bGF\n 1\"]\n    K[label = \"lvl2bGF\n 2\"]\n    N[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    O[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    label4[label = \"\", color=white]\n    label5[label = \"\", color=white]\n    label6[label = \"\", color=white]\n    edge[arrowhead = none]\n    A->B\n    A->C\n    edge[arrowhead = normal]\n    B->F\n    C->G\n    edge[arrowhead = none]\n    I->J\n    I->K\n    edge[arrowhead = normal]\n    J->N\n    K->O\n    edge [color=white]\n    rank=same{A->I}\n    label1->label2->label3\n    label4->label5->label6\n    edge [minlen = 6,color=black,style = dashed,arrowhead=none]\n    rank=same {label1->A}\n    rank=same {I->label4}\n    edge [minlen = 2,color=black,style = dashed,arrowhead=none]\n    rank=same {label2->B}\n    rank=same {K->label5}\n    edge [minlen = 1,color=gray75,style = dashed,arrowhead=none]\n    rank=same {label3->F}\n    rank=same {O->label6}\n  }\n   "
      }
      if (gf_nlevelsA[1] == 3) {
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("R[label = \". . .\",color=white]\n    S[label = \" \",color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("R->S\n    "))
        diagram_text <- stringr::str_replace(diagram_text,
                                             stringr::fixed("rank=same{A->I->R->Q}"),
                                             stringr::fixed("rank=same{A->I->Q}"))
      }
      if (gf_nlevelsA[2] == 3) {
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("D[label = \". . .\", color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("L[label = \". . .\", color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("V[label = \". . .\", color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("A->D\n    I->L\n    "))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("Q->V\n    "))
      }
      if (gf_nlevelsA[2] >= 3 || gf_nlevelsB[2] != "") {
        r2 <- paste("lvl2bGF\n", gf_nlevels[2])
        diagram_text <- stringr::str_replace_all(diagram_text, stringr::fixed("lvl2bGF\n b"), r2)
      }
      if (gf_nlevelsA[1] >= 3 || gf_nlevelsB[1] != "") {
        r1 <- paste("lvl2aGF\n", gf_nlevels[1])
        diagram_text <- stringr::str_replace_all(diagram_text, stringr::fixed("lvl2aGF\n a"), r1)
      }
    }
    diagram_text <- stringr::str_replace_all(diagram_text, "lvl2aGF", gf_cross[1])
    diagram_text <- stringr::str_replace_all(diagram_text, "lvl2bGF", gf_cross[2])

  }else if (n_gf == 3 && gf_description == "crossed") {
    diagram_text <- "\n  digraph {\n    graph [layout = dot,rankdir = TB]\n    edge [color=black]\n    node [shape = box]\n    label0[label = \"Level 2\", color=white]\n    label1[label = \"\", color=white]\n    label2[label = \"\", color=white]\n    label3[label = \"Level 1\", color=white]\n    AA[label = \"lvl2aGF\n i=1,...,a\"]\n    A[label = \"lvl2bGF\n 1\"]\n    B[label = \"lvl2cGF\n 1\"]\n    C[label = \"lvl2cGF\n 2\"]\n    D[label = \". . .\", color=white]\n    E[label = \"lvl2cGF\n c\"]\n    F[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    G[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    H[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    I[label = \"lvl2bGF\n 2\"]\n    J[label = \"lvl2cGF\n 1\"]\n    K[label = \"lvl2cGF\n 2\"]\n    L[label = \". . .\", color=white]\n    M[label = \"lvl2cGF\n c\"]\n    N[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    O[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    P[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    R[label = \". . .\",color=white]\n    S[label = \" \",color=white]\n    Q[label = \"lvl2bGF\n b\"]\n    T[label = \"lvl2cGF\n 1\"]\n    U[label = \"lvl2cGF\n 2\"]\n    V[label = \". . .\", color=white]\n    W[label = \"lvl2cGF\n c\"]\n    X[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    Y[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    Z[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    label35[label = \"\", color=white]\n    label4[label = \"\", color=white]\n    label5[label = \"\", color=white]\n    label6[label = \"\", color=white]\n    edge[arrowhead = none]\n    AA->A\n    AA->I\n    AA->Q\n    A->B\n    A->C\n    A->E\n    edge[arrowhead = normal]\n    B->F\n    C->G\n    E->H\n    edge[arrowhead = none]\n    I->J\n    I->K\n    I->M\n    edge[arrowhead = normal]\n    J->N\n    K->O\n    M->P\n    edge[arrowhead = none]\n    Q->T\n    Q->U\n    Q->W\n    edge[arrowhead = normal]\n    T->X\n    U->Y\n    W->Z\n    edge [color=white]\n    AA->R\n    rank=same{A->I->R->Q}\n    A->D\n    I->L\n    R->S\n    Q->V\n    label0->label1->label2->label3\n    label35->label4->label5->label6\n    edge [minlen = 25,color=black,style = dashed,arrowhead=none]\n    rank=same {label0->AA}\n    rank=same {AA->label35}\n    edge [minlen = 9,color=white,style = dashed,arrowhead=none]\n    rank=same {label1->A}\n    rank=same {Q->label4}\n    edge [minlen = 2,color=black,style = dashed,arrowhead=none]\n    rank=same {label2->B}\n    rank=same {W->label5}\n    edge [minlen = 1,color=gray75,style = dashed,arrowhead=none]\n    rank=same {label3->F}\n    rank=same {Z->label6}\n  }\n   "
    if (use_gf_nlevels == "yes") {
      if (gf_nlevelsA[2] == 3) {
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("R[label = \". . .\",color=white]\n    S[label = \" \",color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("R->S\n    "))
        diagram_text <- stringr::str_replace(diagram_text,
                                             stringr::fixed("rank=same{A->I->R->Q}"),
                                             stringr::fixed("rank=same{A->I->Q}"))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("AA->R\n    "))
      }
      if (gf_nlevelsA[3] == 3) {
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("D[label = \". . .\", color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("L[label = \". . .\", color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("V[label = \". . .\", color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("A->D\n    I->L\n    "))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("Q->V\n    "))
        diagram_text <- stringr::str_replace(diagram_text,
                                             stringr::fixed("minlen = 25"),
                                             stringr::fixed("minlen = 15"))
      }
      if (gf_nlevelsA[3] >= 3 || gf_nlevelsB[3] != "") {
        r3 <- paste("lvl2cGF\n", gf_nlevels[3])
        diagram_text <- stringr::str_replace_all(diagram_text, stringr::fixed("lvl2cGF\n c"), r3)
      }
      if (gf_nlevelsA[2] >= 3 || gf_nlevelsB[2] != "") {
        r2 <- paste("lvl2bGF\n", gf_nlevels[2])
        diagram_text <- stringr::str_replace_all(diagram_text, stringr::fixed("lvl2bGF\n b"), r2)
      }
      r1 <- paste0("lvl2aGF\n i=1,...,", gf_nlevels[1])
      diagram_text <- stringr::str_replace(diagram_text, stringr::fixed("lvl2aGF\n i=1,...,a"), r1)
    }
    if (gf3_index != "i") {
      r1a <- paste("lvl2aGF\n", gf3_index)
      diagram_text <- stringr::str_replace(diagram_text, stringr::fixed("lvl2aGF\n i"), r1a)
    }
    diagram_text <- stringr::str_replace_all(diagram_text, "lvl2aGF", gf_cross[1])
    diagram_text <- stringr::str_replace_all(diagram_text, "lvl2bGF", gf_cross[2])
    diagram_text <- stringr::str_replace_all(diagram_text, "lvl2cGF", gf_cross[3])

  }else if (n_gf == 3 && gf_description == "crossed with nested") {
    diagram_text <- "\n  digraph {\n    graph [layout = dot,rankdir = TB]\n    edge [color=black]\n    node [shape = box]\n    label0[label = \"Level 3\", color=white]\n    label1[label = \"\", color=white]\n    label2[label = \"Level 2\", color=white]\n    label3[label = \"Level 1\", color=white]\n    AA[label = \"lvl3aGF\n i=1,...,a\",shape=oval]\n    A[label = \"lvl3bGF\n 1\",shape=oval]\n    B[label = \"lvl2GF\n 1(1)\"]\n    C[label = \"lvl2GF\n 2(1)\"]\n    D[label = \". . .\", color=white]\n    E[label = \"lvl2GF\n c(1)\"]\n    F[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    G[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    H[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    I[label = \"lvl3bGF\n 2\",shape=oval]\n    J[label = \"lvl2GF\n 1(2)\"]\n    K[label = \"lvl2GF\n 2(2)\"]\n    L[label = \". . .\", color=white]\n    M[label = \"lvl2GF\n c(2)\"]\n    N[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    O[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    P[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    R[label = \". . .\",color=white]\n    S[label = \" \",color=white]\n    Q[label = \"lvl3bGF\n b\",shape=oval]\n    T[label = \"lvl2GF\n 1(b)\"]\n    U[label = \"lvl2GF\n 2(b)\"]\n    V[label = \". . .\", color=white]\n    W[label = \"lvl2GF\n c(b)\"]\n    X[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    Y[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    Z[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    label35[label = \"\", color=white]\n    label4[label = \"\", color=white]\n    label5[label = \"\", color=white]\n    label6[label = \"\", color=white]\n    edge[arrowhead = none]\n    AA->A\n    AA->I\n    AA->Q\n    edge[arrowhead = normal]\n    A->B\n    A->C\n    A->E\n    B->F\n    C->G\n    E->H\n    I->J\n    I->K\n    I->M\n    J->N\n    K->O\n    M->P\n    Q->T\n    Q->U\n    Q->W\n    T->X\n    U->Y\n    W->Z\n    edge [color=white]\n    AA->R\n    rank=same{A->I->R->Q}\n    A->D\n    I->L\n    R->S\n    Q->V\n    label0->label1->label2->label3\n    label35->label4->label5->label6\n    edge [minlen = 26,color=black,style = dashed,arrowhead=none]\n    rank=same {label0->AA}\n    rank=same {AA->label35}\n    edge [minlen = 10,color=black,style = dashed,arrowhead=none]\n    rank=same {label1->A}\n    rank=same {Q->label4}\n    edge [minlen = 2,color=black,style = dashed,arrowhead=none]\n    rank=same {label2->B}\n    rank=same {W->label5}\n    edge [minlen = 2,color=gray75,style = dashed,arrowhead=none]\n    rank=same {label3->F}\n    rank=same {Z->label6}\n  }\n   "
    if (use_gf_nlevels == "yes") {
      if (gf_nlevelsA[2] == 3) {
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("R[label = \". . .\",color=white]\n    S[label = \" \",color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("R->S\n    "))
        diagram_text <- stringr::str_replace(diagram_text,
                                             stringr::fixed("rank=same{A->I->R->Q}"),
                                             stringr::fixed("rank=same{A->I->Q}"))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("AA->R\n    "))
      }
      if (gf_nlevelsA[3] == 3) {
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("D[label = \". . .\", color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("L[label = \". . .\", color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("V[label = \". . .\", color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("A->D\n    I->L\n    "))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("Q->V\n    "))
      }
      if (gf_nlevelsA[2] == 3 && gf_nlevelsA[3] == 3) {
        diagram_text <- stringr::str_replace(diagram_text,
                                             stringr::fixed("minlen = 26"),
                                             stringr::fixed("minlen = 17"))
      }else if (gf_nlevelsA[2] == 3 && gf_nlevelsA[3] != 3) {
        diagram_text <- stringr::str_replace(diagram_text,
                                             stringr::fixed("minlen = 26"),
                                             stringr::fixed("minlen = 20"))
      } else if(gf_nlevelsA[2] != 3 && gf_nlevelsA[3] == 3) {
        diagram_text <- stringr::str_replace(diagram_text,
                                             stringr::fixed("minlen = 26"),
                                             stringr::fixed("minlen = 17"))
      }
      if (gf_nlevelsA[3] >= 3 || gf_nlevelsB[3] != "") {
        r3 <- paste("lvl2GF\n", gf_nlevels[3])
        diagram_text <- stringr::str_replace_all(diagram_text, stringr::fixed("lvl2GF\n c"), r3)
      }
      if (gf_nlevelsA[2] >= 3 || gf_nlevelsB[2] != "") {
        r2a <- paste("lvl3bGF\n", gf_nlevels[2])
        diagram_text <- stringr::str_replace_all(diagram_text, stringr::fixed("lvl3bGF\n b"), r2a)
        r2b <- paste0("(", gf_nlevels[2], ")")
        diagram_text <- stringr::str_replace_all(diagram_text, stringr::fixed("(b)"), r2b)
      }
      r1 <- paste0("lvl3aGF\n i=1,...,", gf_nlevels[1])
      diagram_text <- stringr::str_replace(diagram_text, stringr::fixed("lvl3aGF\n i=1,...,a"), r1)
    }
    if (gf3_index != "i") {
      r1a <- paste("lvl3aGF\n", gf3_index)
      diagram_text <- stringr::str_replace(diagram_text, stringr::fixed("lvl3aGF\n i"), r1a)
    }
    diagram_text <- stringr::str_replace_all(diagram_text, "lvl3aGF", gf_cross[1])
    diagram_text <- stringr::str_replace_all(diagram_text, "lvl3bGF", gf_nest[1])
    diagram_text <- stringr::str_replace_all(diagram_text, "lvl2GF", gf_nest[2])

  }else if (n_gf == 3 && gf_description == "crossed within nested") {
    diagram_text <- "\n  digraph {\n    graph [layout = dot,rankdir = TB]\n    edge [color=black]\n    node [shape = box]\n    label0[label = \"Level 3\", color=white]\n    label1[label = \"Level 2\", color=white]\n    label2[label = \"\", color=white]\n    label3[label = \"Level 1\", color=white]\n    AA[label = \"lvl3GF\n i=1,...,a\",shape=oval]\n    A[label = \"lvl2aGF\n 1(i)\"]\n    B[label = \"lvl2bGF\n 1(i)\"]\n    C[label = \"lvl2bGF\n 2(i)\"]\n    D[label = \". . .\", color=white]\n    E[label = \"lvl2bGF\n c(i)\"]\n    F[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    G[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    H[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    I[label = \"lvl2aGF\n 2(i)\"]\n    J[label = \"lvl2bGF\n 1(i)\"]\n    K[label = \"lvl2bGF\n 2(i)\"]\n    L[label = \". . .\", color=white]\n    M[label = \"lvl2bGF\n c(i)\"]\n    N[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    O[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    P[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    R[label = \". . .\",color=white]\n    S[label = \" \",color=white]\n    Q[label = \"lvl2aGF\n b(i)\"]\n    T[label = \"lvl2bGF\n 1(i)\"]\n    U[label = \"lvl2bGF\n 2(i)\"]\n    V[label = \". . .\", color=white]\n    W[label = \"lvl2bGF\n c(i)\"]\n    X[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    Y[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    Z[label = \"1\n2\n.\n.\n.\nn\",color=gray75,fontsize=10,width = 0.5]\n    label35[label = \"\", color=white]\n    label4[label = \"\", color=white]\n    label5[label = \"\", color=white]\n    label6[label = \"\", color=white]\n    AA->A\n    AA->I\n    AA->Q\n    edge[arrowhead = none]\n    A->B\n    A->C\n    A->E\n    edge[arrowhead = normal]\n    B->F\n    C->G\n    E->H\n    edge[arrowhead = none]\n    I->J\n    I->K\n    I->M\n    edge[arrowhead = normal]\n    J->N\n    K->O\n    M->P\n    edge[arrowhead = none]\n    Q->T\n    Q->U\n    Q->W\n    edge[arrowhead = normal]\n    T->X\n    U->Y\n    W->Z\n    edge [color=white]\n    AA->R\n    rank=same{A->I->R->Q}\n    A->D\n    I->L\n    R->S\n    Q->V\n    label0->label1->label2->label3\n    label35->label4->label5->label6\n    edge [minlen = 26,color=black,style = dashed,arrowhead=none]\n    rank=same {label0->AA}\n    rank=same {AA->label35}\n    edge [minlen = 10,color=black,style = dashed,arrowhead=none]\n    rank=same {label1->A}\n    rank=same {Q->label4}\n    edge [minlen = 2,color=black,style = dashed,arrowhead=none]\n    rank=same {label2->B}\n    rank=same {W->label5}\n    edge [minlen = 1,color=gray75,style = dashed,arrowhead=none]\n    rank=same {label3->F}\n    rank=same {Z->label6}\n  }\n   "
    if (use_gf_nlevels == "yes") {
      if (gf_nlevelsA[2] == 3) {
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("R[label = \". . .\",color=white]\n    S[label = \" \",color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("R->S\n    "))
        diagram_text <- stringr::str_replace(diagram_text,
                                             stringr::fixed("rank=same{A->I->R->Q}"),
                                             stringr::fixed("rank=same{A->I->Q}"))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("AA->R\n    "))
      }
      if (gf_nlevelsA[3] == 3) {
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("D[label = \". . .\", color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("L[label = \". . .\", color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text,
                                            stringr::fixed("V[label = \". . .\", color=white]\n    "))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("A->D\n    I->L\n    "))
        diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("Q->V\n    "))
      }
      if (gf_nlevelsA[2] == 3 && gf_nlevelsA[3] == 3) {
        diagram_text <- stringr::str_replace(diagram_text,
                                             stringr::fixed("minlen = 26"),
                                             stringr::fixed("minlen = 17"))
      }else if (gf_nlevelsA[2] == 3 && gf_nlevelsA[3] != 3) {
        diagram_text <- stringr::str_replace(diagram_text,
                                             stringr::fixed("minlen = 26"),
                                             stringr::fixed("minlen = 20"))
      } else if(gf_nlevelsA[2] != 3 && gf_nlevelsA[3] == 3) {
        diagram_text <- stringr::str_replace(diagram_text,
                                             stringr::fixed("minlen = 26"),
                                             stringr::fixed("minlen = 17"))
      }
      if (gf_nlevelsA[3] >= 3 || gf_nlevelsB[3] != "") {
        r3 <- paste("lvl2bGF\n", gf_nlevels[3])
        diagram_text <- stringr::str_replace_all(diagram_text, stringr::fixed("lvl2bGF\n c"), r3)
      }
      if (gf_nlevelsA[2] >= 3 || gf_nlevelsB[2] != "") {
        r2 <- paste("lvl2aGF\n", gf_nlevels[2])
        diagram_text <- stringr::str_replace_all(diagram_text, stringr::fixed("lvl2aGF\n b"), r2)
      }
      r1 <- paste0("lvl3GF\n i=1,...,", gf_nlevels[1])
      diagram_text <- stringr::str_replace(diagram_text, stringr::fixed("lvl3GF\n i=1,...,a"), r1)
    }
    if (gf3_index != "i") {
      r1a <- paste("lvl3GF\n", gf3_index)
      diagram_text <- stringr::str_replace(diagram_text, stringr::fixed("lvl3GF\n i"), r1a)
      r1b <- paste0("(",gf3_index, ")")
      diagram_text <- stringr::str_replace_all(diagram_text, stringr::fixed("(i)"), r1b)
    }
    diagram_text <- stringr::str_replace_all(diagram_text, "lvl3GF", gf_nest[1])
    diagram_text <- stringr::str_replace_all(diagram_text, "lvl2aGF", gf_cross[1])
    diagram_text <- stringr::str_replace_all(diagram_text, "lvl2bGF", gf_cross[2])
  }

  if (label_levels == "no") {
    diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("Level 1"))
    diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("Level 2"))
    diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("Level 3"))
    diagram_text <- stringr::str_remove(diagram_text, stringr::fixed("Level 4"))
  }

  if (export_type == "print") {
    DiagrammeR::grViz(diagram_text)
  }else if (export_type == "text") {
    diagram_text
  }else if (export_type == "png") {
    dt <- DiagrammeR::grViz(diagram_text)
    rsvg::rsvg_png(svg = charToRaw(DiagrammeRsvg::export_svg(dt)),
                   file = "extract_structure.png",
                   height = 500)
  }
}
