get_all_auc_labels = function() {
    labels = c(
        "auc_g_hat",
        "auc_g_hat_strict",
        "auc_g_hat_loose",
        "auc_tc_strict",
        "auc_tc_loose",
        "pag",
        "cpdag"
    )
    labels = str_to_lower(labels)
    return( labels )
}