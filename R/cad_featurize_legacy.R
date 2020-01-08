
#' Featurize pairwise scatterplots
#'
#' @param X Data
#' @param G0 Graph with entries {1, 2} corresponding to {0, 1} and 0 meaning unknown
#' @param q Number of PCA components to keep
#' @param block_size Do the PCA on the first block only, the rest is projected
#' @param feature_extractor Feature extractor function to summarize the joint marginals of X1, ..., Xp
#' @param parallel Run in parallel
#' @param verbosity After how many iterations to print
#'
#' @return Feature matrix (F) and corresponding labels (Y)
#' @export
#'
cad_featurize_legacy = function(
    X, G0,
    q = 100, block_size = 500,
    feature_extractor = feature_extractor_hist2d,
    parallel = TRUE,
    verbosity = 10
) {
    
    stopifnot( ncol(X) == ncol(G0) )

    X = scale(X)
    stopifnot( !anyNA(X) )

    n = nrow(X)
    p = ncol(X)

    # int_count is number of interventions, including those entirely unseen (if any)
    int_count = nrow(G0)

    # Permutation
    perm = sample.int(p)

    # Work in blocks
    block_count = ceiling(p / block_size)

    # Number of pairs for linear indexing
    pair_count = int_count * p

    # Length of feature vector
    F_test_run = feature_extractor( matrix(runif(30*2), 30) )
    stopifnot( is_vector(F_test_run) )
    features_count = length(F_test_run)

    # Create feature matrix (F) in blocks.
    # At the same time store the index mapping (K) and the response (Y)
    if (parallel) {

        setMKLthreads(1)

        num_cores = min(block_count - 1, detectCores() - 1)
        cl = makeCluster(num_cores)
        registerDoParallel(cl)

        # featurisation for the first block
        if (verbosity != 0) {
            cat('* Featurisation: block 1 ...')
        }
        
        p1 = 1
        p2 = min(p1 + block_size - 1, p)
        pair_block_count = (p2 - p1 + 1) * int_count

        F_block = matrix(NA, pair_block_count, features_count)
        K_block = matrix(NA, pair_block_count, 3)
        colnames(K_block) = c("ij", "i", "j")
        Y_block = matrix(NA, pair_block_count, 1)

        # Compute the feature matrix
        ij = 1
        for (ii in 1:int_count) {
            i = ii
            for (jj in p1:p2) {
                j = perm[jj]

                Xij = X[, c(i, j)]
                H   = feature_extractor(Xij)

                F_block[ij, ] = H
                K_block[ij, ] = c(ij, i, j)
                Y_block[ij]   = G0[i,j]

                ij = ij + 1
            } # for jj
        } # for ii

        K = K_block
        Y = Y_block

        # Do the PCA only on the first block!
        pca_res = pcaMethods::pca(F_block, nPcs = q, center = TRUE, scale = "none")
        WW = pca_res@loadings
        ZZ = pca_res@scores
        F  = ZZ
        F_wide = F_block


        # Do featurisation for all other blocks in parallel

        if ((verbosity != 0) && (b %% verbosity == 0)) {
            cat('  First block done. \n Doing the remaining ones in parallel on ', num_cores,' cores... \n')
        }
        
        par_results <- foreach(b = 2:block_count,
                               .export = c('predictor_keras'),
                               .packages = c('gplots', 'MASS', 'pcaMethods',
                                             'glmnet', 'keras'),
                               .combine = rbind, .multicombine=TRUE,
                               .init = vector(mode='list', 4)) %dopar% {

                                   p1 = block_size * (b - 1) + 1
                                   p2 = min(p1 + block_size - 1, p)
                                   pair_block_count = (p2 - p1 + 1) * int_count

                                   F_block = matrix(NA, pair_block_count, features_count)
                                   K_block = matrix(NA, pair_block_count, 3)
                                   colnames(K_block) = c("ij", "i", "j")
                                   Y_block = matrix(NA, pair_block_count, 1)

                                   # Compute the feature matrix
                                   ij = 1
                                   for (ii in 1:int_count) {
                                       i = ii
                                       for (jj in p1:p2) {
                                           j = perm[jj]

                                           Xij = X[, c(i, j)]
                                           H = feature_extractor(Xij)

                                           F_block[ij, ] = H
                                           K_block[ij, ] = c(ij, i, j)
                                           Y_block[ij]   = G0[i,j]

                                           ij = ij + 1
                                       } # for jj
                                   } # for ii

                                   # PCA projection of feature block
                                   F = scale(F_block, center = TRUE, scale = FALSE) %*% WW;

                                   return(
                                       list(F, K_block, Y_block, F_block)
                                    )
                               }  # foreach

        stopCluster(cl)
        setMKLthreads()
        closeAllConnections()

        # combine results from parallel computations
        for(i in 2:block_count){
            F <- rbind(F, par_results[[i,1]])
            K <- rbind(K, par_results[[i,2]])
            Y <- rbind(Y, par_results[[i,3]])
            F_wide <- rbind(F_wide, par_results[[i,4]])
        }


    } else {
        # compute feature matrix in series
        F_wide = matrix(NA, pair_count, features_count)
        F = matrix(NA, pair_count, q)
        K = matrix(NA, pair_count, 3)
        colnames(K) = c("ij", "i", "j")
        Y = matrix(NA, pair_count, 1)
        start_idx = 1

        for (b in 1:block_count) {
            p1 = block_size * (b - 1) + 1
            p2 = min(p1 + block_size - 1, p)
            pair_block_count = (p2 - p1 + 1) * int_count

            F_block = matrix(NA, pair_block_count, features_count)
            K_block = matrix(NA, pair_block_count, 3)
            colnames(K_block) = c("ij", "i", "j")
            Y_block = matrix(NA, pair_block_count, 1)

            # Compute the feature matrix
            ij = 1
            for (ii in 1:int_count) {
                i = ii
                for (jj in p1:p2) {
                    j = perm[jj]

                    Xij = X[, c(i, j)]
                    H = feature_extractor(Xij)

                    F_block[ij, ] = H
                    K_block[ij, ] = c(ij, i, j)
                    Y_block[ij]   = G0[i,j]

                    ij = ij + 1
                } # for jj
            } # for ii

            # Display block number
            if ((verbosity != 0) && (b %% verbosity == 0)) {
                cat('* Block n.', b, '\n')
            }

            # Do the PCA only on the first block!
            if (b == 1) {
                #pca = prcomp(F_block, rank = q)
                #WW = pca$rotation
                #ZZ = scale(F_block, center = TRUE, scale = FALSE) %*% WW
                pca_res = pcaMethods::pca(F_block, nPcs = q, center = TRUE, scale = "none")
                WW = pca_res@loadings
                ZZ = pca_res@scores
                F[start_idx:(start_idx+pair_block_count-1), ] = ZZ;
            } else {
                F[start_idx:(start_idx+pair_block_count-1), ] = scale(F_block, center = TRUE, scale = FALSE) %*% WW;
            }

            # Append block-wise computation to the complete matrix
            K[start_idx:(start_idx+pair_block_count-1), ]      = K_block;
            Y[start_idx:(start_idx+pair_block_count-1)]        = Y_block;
            F_wide[start_idx:(start_idx+pair_block_count-1), ] = F_block;

            # We don't need to remove the variables actually, as they are iteratively
            # reassigned to a zero matrix
            # rm(list = c("F_block", "K_block", "Y_block"))

            start_idx = start_idx + pair_block_count;
            
        } # for blocks
    }

    if ((verbosity != 0) && (b %% verbosity == 0)) {
        print("Featurization complete!")
    }

    return(
        list(G0 = G0, F = F, Y = Y, K = K, F_wide = F_wide)
    )

}
