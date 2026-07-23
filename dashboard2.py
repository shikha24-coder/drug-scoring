import streamlit as st
import pandas as pd
import duckdb
import plotly.express as px
import plotly.graph_objects as go

# =====================================================
# CONFIG
# =====================================================

PARQUET_FILE = r"C:\Users\shikha1.roy\Downloads\merged_drug_data.parquet"

st.set_page_config(
    page_title="Drug Discovery Dashboard",
    layout="wide"
)

st.title("🔬 Drug Discovery Dashboard")

# =====================================================
# LOAD DATA
# =====================================================

@st.cache_data
def load_data():
    query = f"""
    SELECT *
    FROM read_parquet('{PARQUET_FILE}')
    LIMIT 100000
    """
    return duckdb.sql(query).df()

df = load_data()

st.success(f"Loaded {len(df):,} rows")
# =====================================================
# LOAD CLINICAL + ADVERSE DATA
# =====================================================

CLINICAL_PARQUET = (
    r"C:\Users\shikha1.roy\Downloads\adverse_clinical_combined.parquet"
)

@st.cache_data
def load_clinical_data():

    return pd.read_parquet(
        CLINICAL_PARQUET,
        engine="pyarrow"
    )

clinical_df = load_clinical_data()

st.success(
    f"Loaded {len(clinical_df):,} Clinical + Adverse Event records"
)

# =====================================================
# COLUMN MAPPING
# =====================================================

st.sidebar.header("Column Mapping")

columns = df.columns.tolist()

drug_col = st.sidebar.selectbox(
    "Drug Column",
    columns,
    index=columns.index("drug_name")
    if "drug_name" in columns else 0
)

disease_col = st.sidebar.selectbox(
    "Disease Column",
    columns
)

target_col = st.sidebar.selectbox(
    "Target Column",
    columns
)

# =====================================================
# VALIDATION
# =====================================================

if disease_col == target_col:
    st.error(
        "Disease Column and Target Column cannot be the same."
    )
    st.stop()

# =====================================================
# FILTERS
# =====================================================

st.sidebar.header("Filters")

drug_values = sorted(
    df[drug_col]
    .dropna()
    .astype(str)
    .unique()
    .tolist()
)

selected_drugs = st.sidebar.multiselect(
    "Select Drug",
    drug_values
)

filtered_df = df.copy()

if selected_drugs:
    filtered_df = filtered_df[
        filtered_df[drug_col]
        .astype(str)
        .isin(selected_drugs)
    ]

search_text = st.sidebar.text_input(
    "Search Drug"
)

if search_text:
    filtered_df = filtered_df[
        filtered_df[drug_col]
        .astype(str)
        .str.contains(
            search_text,
            case=False,
            na=False
        )
    ]

# =====================================================
# SUMMARY
# =====================================================

c1, c2 = st.columns(2)

with c1:
    st.metric(
        "Rows",
        f"{len(filtered_df):,}"
    )

with c2:
    st.metric(
        "Columns",
        len(filtered_df.columns)
    )

# =====================================================
# PREVIEW
# =====================================================

st.header("Preview")

st.dataframe(
    filtered_df.head(1000),
    use_container_width=True
)

# =====================================================
# BUILD RELATIONSHIP TABLE
# =====================================================

relation_df = filtered_df[
    [disease_col, target_col]
].copy()

relation_df.columns = [
    "Disease",
    "Target"
]

relation_df = relation_df.dropna()

relation_df["Disease"] = (
    relation_df["Disease"]
    .astype(str)
    .str.strip()
)

relation_df["Target"] = (
    relation_df["Target"]
    .astype(str)
    .str.strip()
)

relation_df = relation_df[
    (relation_df["Disease"] != "")
    &
    (relation_df["Target"] != "")
]

relation_df = relation_df.drop_duplicates()

st.write(
    f"Disease-Target Relationships: {len(relation_df):,}"
)

if len(relation_df) == 0:
    st.warning(
        "No Disease-Target relationships found."
    )
    st.stop()

# =====================================================
# RELATIONSHIP COUNTS
# =====================================================

graph_df = (
    relation_df
    .groupby(
        ["Disease", "Target"]
    )
    .size()
    .reset_index(name="count")
    .sort_values(
        "count",
        ascending=False
    )
    .head(100)
)

# =====================================================
# SANKEY
# =====================================================

st.header("Disease → Target Sankey")

labels = list(
    pd.concat(
        [
            graph_df["Disease"],
            graph_df["Target"]
        ]
    ).unique()
)

source = [
    labels.index(x)
    for x in graph_df["Disease"]
]

target = [
    labels.index(x)
    for x in graph_df["Target"]
]

fig_sankey = go.Figure(
    go.Sankey(
        node=dict(
            label=labels,
            pad=15,
            thickness=20
        ),
        link=dict(
            source=source,
            target=target,
            value=graph_df["count"]
        )
    )
)

st.plotly_chart(
    fig_sankey,
    use_container_width=True
)

# =====================================================
# SUNBURST
# =====================================================

st.header("Disease → Target Sunburst")

fig_sunburst = px.sunburst(
    graph_df,
    path=["Disease", "Target"],
    values="count"
)

st.plotly_chart(
    fig_sunburst,
    use_container_width=True
)

# =====================================================
# TREEMAP
# =====================================================

st.header("Disease → Target Treemap")

fig_treemap = px.treemap(
    graph_df,
    path=["Disease", "Target"],
    values="count"
)

st.plotly_chart(
    fig_treemap,
    use_container_width=True
)

# =====================================================
# HEATMAP
# =====================================================

st.header("Disease ↔ Target Heatmap")

fig_heatmap = px.density_heatmap(
    graph_df,
    x="Target",
    y="Disease",
    z="count",
    color_continuous_scale="Viridis"
)

st.plotly_chart(
    fig_heatmap,
    use_container_width=True
)

# =====================================================
# TOP DISEASES
# =====================================================

st.header("Top Diseases")

top_disease = (
    relation_df["Disease"]
    .value_counts()
    .head(20)
    .reset_index()
)

top_disease.columns = [
    "Disease",
    "Count"
]

fig_disease = px.pie(
    top_disease,
    names="Disease",
    values="Count"
)

st.plotly_chart(
    fig_disease,
    use_container_width=True
)

# =====================================================
# TOP TARGETS
# =====================================================

st.header("Top Targets")

top_target = (
    relation_df["Target"]
    .value_counts()
    .head(20)
    .reset_index()
)

top_target.columns = [
    "Target",
    "Count"
]

fig_target = px.treemap(
    top_target,
    path=["Target"],
    values="Count"
)

st.plotly_chart(
    fig_target,
    use_container_width=True
)


# ==========================================================
# CLINICAL DEVELOPMENT ANALYTICS
# ==========================================================

st.header("🧪 Clinical Development Analytics")

if "clinical_stage" in clinical_df.columns:

    clinical_stage_df = (
        clinical_df["clinical_stage"]
        .dropna()
        .astype(str)
        .value_counts()
        .reset_index()
    )

    clinical_stage_df.columns = ["Clinical Stage", "Count"]

    fig = px.funnel(
        clinical_stage_df,
        x="Count",
        y="Clinical Stage"
    )

    st.plotly_chart(fig, use_container_width=True)

if "trial_phase" in clinical_df.columns:

    phase_df = (
        clinical_df["trial_phase"]
        .dropna()
        .astype(str)
        .value_counts()
        .reset_index()
    )

    phase_df.columns = [
        "Trial Phase",
        "Count"
    ]

    fig = px.pie(
        phase_df,
        names="Trial Phase",
        values="Count",
        hole=0.4
    )

    st.plotly_chart(
        fig,
        use_container_width=True
    )

if "trial_study_type" in clinical_df.columns:

    study_df = (
        clinical_df["trial_study_type"]
        .dropna()
        .astype(str)
        .value_counts()
        .head(20)
        .reset_index()
    )

    study_df.columns = [
        "Study Type",
        "Count"
    ]

    fig = px.treemap(
        study_df,
        path=["Study Type"],
        values="Count"
    )

    st.plotly_chart(
        fig,
        use_container_width=True
    )

# ==========================================================
# ADVERSE EVENT ANALYTICS
# ==========================================================
# ==========================================================
# RISK SCORE MODEL
# ==========================================================

st.header("🚨 Drug Risk Score Analytics")

risk_df = clinical_df.copy()

risk_df["report_count"] = pd.to_numeric(
    risk_df["report_count"],
    errors="coerce"
).fillna(0)

risk_df["log_likelihood_ratio"] = pd.to_numeric(
    risk_df["log_likelihood_ratio"],
    errors="coerce"
).fillna(0)

stage_weights = {
    "APPROVAL": 1,
    "PHASE_4": 2,
    "PHASE_3": 3,
    "PHASE_2": 4,
    "PHASE_1": 5,
    "EARLY_PHASE_1": 5,
    "UNKNOWN": 3
}

risk_df["stage_score"] = (
    risk_df["clinical_stage"]
    .astype(str)
    .map(stage_weights)
    .fillna(3)
)

risk_df["report_norm"] = (
    risk_df["report_count"]
    / risk_df["report_count"].max()
)

risk_df["llr_norm"] = (
    risk_df["log_likelihood_ratio"]
    / risk_df["log_likelihood_ratio"].max()
)

risk_df["stage_norm"] = (
    risk_df["stage_score"]
    / risk_df["stage_score"].max()
)

risk_df["risk_score"] = (
    0.50 * risk_df["llr_norm"]
    + 0.30 * risk_df["report_norm"]
    + 0.20 * risk_df["stage_norm"]
)

drug_risk = (
    risk_df
    .groupby("query_name_ae")
    .agg(
        Risk_Score=("risk_score", "mean"),
        Reports=("report_count", "sum"),
        Avg_LLR=("log_likelihood_ratio", "mean")
    )
    .reset_index()
)

drug_risk = drug_risk.sort_values(
    "Risk_Score",
    ascending=False
)

# Top Risk Drugs
fig = px.bar(
    drug_risk.head(20),
    x="Risk_Score",
    y="query_name_ae",
    orientation="h",
    color="Risk_Score",
    color_continuous_scale="Reds",
    title="Top High-Risk Drugs"
)

fig.update_layout(height=700)

st.plotly_chart(
    fig,
    use_container_width=True
)

# Risk Matrix
fig = px.scatter(
    drug_risk,
    x="Reports",
    y="Avg_LLR",
    size="Risk_Score",
    color="Risk_Score",
    hover_name="query_name_ae",
    color_continuous_scale="Reds",
    title="Risk Matrix"
)

st.plotly_chart(
    fig,
    use_container_width=True
)

# Risk Category

drug_risk["Risk Category"] = pd.cut(
    drug_risk["Risk_Score"],
    bins=[0, 0.25, 0.50, 0.75, 1.00],
    labels=[
        "Low",
        "Moderate",
        "High",
        "Critical"
    ]
)

risk_summary = (
    drug_risk["Risk Category"]
    .value_counts()
    .reset_index()
)

risk_summary.columns = [
    "Risk Category",
    "Count"
]

fig = px.pie(
    risk_summary,
    names="Risk Category",
    values="Count",
    title="Risk Category Distribution"
)

st.plotly_chart(
    fig,
    use_container_width=True
)

st.subheader("Top Drug Risk Rankings")

st.dataframe(
    drug_risk.head(50),
    use_container_width=True
)
st.header("⚠️ Adverse Event Analytics")

clinical_df["report_count"] = pd.to_numeric(
    clinical_df["report_count"],
    errors="coerce"
)

clinical_df["log_likelihood_ratio"] = pd.to_numeric(
    clinical_df["log_likelihood_ratio"],
    errors="coerce"
)

adverse_summary = (
    clinical_df
    .groupby("adverse_event_name")
    .agg(
        report_count=("report_count", "sum"),
        log_likelihood_ratio=("log_likelihood_ratio", "mean")
    )
    .reset_index()
)

adverse_summary = (
    adverse_summary
    .sort_values(
        "report_count",
        ascending=False
    )
    .head(25)
)

st.subheader("Statistics")

c1, c2, c3, c4 = st.columns(4)

with c1:
    st.metric(
        "Unique Events",
        adverse_summary["adverse_event_name"].nunique()
    )

with c2:
    st.metric(
        "Total Reports",
        f"{int(adverse_summary['report_count'].sum()):,}"
    )

with c3:
    st.metric(
        "Average Reports",
        round(
            adverse_summary["report_count"].mean(),
            2
        )
    )

with c4:
    st.metric(
        "Max LLR",
        round(
            adverse_summary[
                "log_likelihood_ratio"
            ].max(),
            2
        )
    )

fig = px.bar(
    adverse_summary.sort_values(
        "report_count"
    ),
    x="report_count",
    y="adverse_event_name",
    orientation="h",
    color="report_count",
    color_continuous_scale="Reds"
)

fig.update_layout(height=700)

st.plotly_chart(
    fig,
    use_container_width=True
)

fig = px.scatter(
    adverse_summary,
    x="report_count",
    y="log_likelihood_ratio",
    text="adverse_event_name",
    size="report_count",
    color="log_likelihood_ratio"
)

st.plotly_chart(
    fig,
    use_container_width=True
)

fig = px.histogram(
    adverse_summary,
    x="report_count",
    nbins=30
)

st.plotly_chart(
    fig,
    use_container_width=True
)

fig = px.box(
    adverse_summary,
    y="log_likelihood_ratio",
    points="outliers"
)

st.plotly_chart(
    fig,
    use_container_width=True
)

st.dataframe(
    adverse_summary,
    use_container_width=True
)

# ==========================================================
# DRUG → ADVERSE EVENT SANKEY
# ==========================================================

if (
    "query_name_ae" in clinical_df.columns
    and "adverse_event_name" in clinical_df.columns
):

    st.header("💊 Drug → Adverse Event Sankey")

    sankey_df = (
        clinical_df[
            [
                "query_name_ae",
                "adverse_event_name"
            ]
        ]
        .dropna()
        .astype(str)
        .drop_duplicates()
        .head(100)
    )

    labels = list(
        pd.concat(
            [
                sankey_df["query_name_ae"],
                sankey_df["adverse_event_name"]
            ]
        ).unique()
    )

    source = [
        labels.index(x)
        for x in sankey_df["query_name_ae"]
    ]

    target = [
        labels.index(x)
        for x in sankey_df["adverse_event_name"]
    ]

    sankey_fig = go.Figure(
        go.Sankey(
            node=dict(
                label=labels,
                pad=15,
                thickness=20
            ),
            link=dict(
                source=source,
                target=target,
                value=[1] * len(source)
            )
        )
    )

    st.plotly_chart(
        sankey_fig,
        use_container_width=True
    )

# =====================================================
# DOWNLOAD
# =====================================================

st.header("Download")

csv = filtered_df.head(
    min(50000, len(filtered_df))
).to_csv(index=False)

st.download_button(
    "Download Filtered CSV",
    csv,
    "filtered_data.csv",
    "text/csv"
)

# =====================================================
# DEBUG
# =====================================================

with st.expander("Available Columns"):
    st.write(df.columns.tolist())