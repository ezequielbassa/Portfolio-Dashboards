"""
Workplace Climate Survey Generator
Maritime Logistics Company - Americas Operations
Generates: workplace_climate_maritime_2026.csv (12,500 records)
"""

import pandas as pd
import numpy as np
from faker import Faker

# ─── Reproducibility ──────────────────────────────────────────────────────────
SEED = 42
np.random.seed(SEED)
fake = Faker("en_US")
Faker.seed(SEED)

N = 12_500
CHUNK_SIZE = 2_500

# ─── Reference Data ───────────────────────────────────────────────────────────
REGION_COUNTRIES = {
    "North America":   ["USA", "Canada", "Mexico"],
    "Central America": ["Panama", "Costa Rica", "Guatemala", "Honduras"],
    "South America":   ["Brazil", "Chile", "Argentina", "Peru", "Colombia"],
}

REGION_WEIGHTS = [0.40, 0.20, 0.40]   # ~60% coastal presence weighted

DEPARTMENTS = [
    "Operations-Ports",
    "Logistics-Trucking",
    "IT",
    "Sales/Customer Service",
    "HR",
    "Maritime-Crew",
]

DEPT_WEIGHTS = [0.25, 0.20, 0.10, 0.15, 0.08, 0.22]

WORK_TYPE_BY_DEPT = {
    "Operations-Ports":      {"On-site": 0.85, "Remote": 0.10, "Offshore/Vessel": 0.05},
    "Logistics-Trucking":    {"On-site": 0.80, "Remote": 0.15, "Offshore/Vessel": 0.05},
    "IT":                    {"On-site": 0.30, "Remote": 0.65, "Offshore/Vessel": 0.05},
    "Sales/Customer Service":{"On-site": 0.50, "Remote": 0.45, "Offshore/Vessel": 0.05},
    "HR":                    {"On-site": 0.55, "Remote": 0.40, "Offshore/Vessel": 0.05},
    "Maritime-Crew":         {"On-site": 0.05, "Remote": 0.02, "Offshore/Vessel": 0.93},
}

GENDER = ["Male", "Female", "Non-binary"]
GENDER_WEIGHTS = [0.58, 0.38, 0.04]

SALARY_BY_DEPT = {
    "Operations-Ports":      {"Junior": 0.30, "Mid": 0.40, "Senior": 0.25, "Executive": 0.05},
    "Logistics-Trucking":    {"Junior": 0.35, "Mid": 0.40, "Senior": 0.20, "Executive": 0.05},
    "IT":                    {"Junior": 0.20, "Mid": 0.35, "Senior": 0.35, "Executive": 0.10},
    "Sales/Customer Service":{"Junior": 0.30, "Mid": 0.40, "Senior": 0.25, "Executive": 0.05},
    "HR":                    {"Junior": 0.25, "Mid": 0.45, "Senior": 0.25, "Executive": 0.05},
    "Maritime-Crew":         {"Junior": 0.40, "Mid": 0.35, "Senior": 0.20, "Executive": 0.05},
}

CLIMATE_COLS = [
    "eNPS", "Safety_Culture", "Cross_Border_Collab", "Leadership_Trust",
    "Work_Life_Balance", "Career_Growth", "Inclusion_Diversity", "Operational_Stress",
    "Fair_Compensation", "Tech_Adequacy", "Recognition", "Environmental_Pride",
    "Engagement", "Job_Security", "Managerial_Support", "Ethical_Standards",
    "Training_Quality", "Communication_Clarity", "Team_Cohesion", "Retention_Intent",
]

# ─── Open Comments Templates ──────────────────────────────────────────────────
POSITIVE_COMMENTS = [
    "The safety protocols on board have improved significantly this year. Leadership is responsive and I feel supported in my role. Overall morale across my team is strong.",
    "Cross-department collaboration has gotten better with the new project management tools. My manager genuinely listens to feedback. I feel proud to work for a company committed to sustainable operations.",
    "Training programs are well-structured and directly applicable to my daily tasks. I see a clear path for career advancement. The team culture here is one of the best I have experienced.",
    "Leadership communicates openly about company direction and challenges. Compensation feels fair given my experience and contributions. I appreciate the flexibility offered for administrative staff.",
    "The company's environmental initiatives are something I genuinely believe in. My colleagues are collaborative and professional. I feel secure in my position and motivated to grow here.",
]

NEUTRAL_COMMENTS = [
    "Work schedules could be more predictable, especially during high-season port congestion. Recognition programs exist but feel inconsistent across departments. Better inter-regional communication would improve coordination.",
    "IT systems are adequate but aging in some operational areas. Career growth discussions happen but follow-through on promised timelines is uneven. The team is good, though cross-border alignment can be slow.",
    "There are opportunities here but navigating them requires initiative. Safety culture is solid in my area; I cannot speak for all ports. Compensation is acceptable though not leading for the industry.",
    "Management is well-intentioned but communication about strategic changes can lag. Benefits are competitive. I would recommend this company to others with realistic expectations about the industry pace.",
    "My direct manager is supportive, though upper leadership feels distant from daily operational realities. Training quality varies depending on location. Job security feels reasonable given current freight market conditions.",
]

NEGATIVE_COMMENTS = [
    "Rotation schedules at sea leave almost no time for personal life. I have raised concerns about fatigue to supervisors but little has changed. Retention of experienced crew is becoming a serious problem.",
    "Workload distribution is inequitable across teams and it is rarely addressed by management. Recognition is reserved for high-visibility projects and overlooked for consistent daily performance. I am actively looking for other opportunities.",
    "Promised career development discussions have not materialized despite multiple requests. The technology used in my department is outdated and slows down operations. Morale has dropped noticeably over the past year.",
    "Safety incidents are sometimes underreported due to pressure to maintain schedules. This is a serious cultural issue that needs leadership attention. I do not feel my concerns are taken seriously.",
    "Compensation has not kept pace with inflation or industry benchmarks. Management turnover has been disruptive and has eroded team trust. I lack confidence in the company's long-term direction.",
]


def pick_comment(sentiment_score: float) -> str:
    """Return a themed comment based on average sentiment score."""
    if sentiment_score >= 3.8:
        return np.random.choice(POSITIVE_COMMENTS)
    elif sentiment_score >= 2.8:
        return np.random.choice(NEUTRAL_COMMENTS)
    else:
        return np.random.choice(NEGATIVE_COMMENTS)


# ─── Core Generation ──────────────────────────────────────────────────────────

def assign_region_country(n: int) -> tuple[list, list]:
    regions = np.random.choice(
        list(REGION_COUNTRIES.keys()), size=n, p=REGION_WEIGHTS
    )
    countries = [
        np.random.choice(REGION_COUNTRIES[r]) for r in regions
    ]
    return regions.tolist(), countries


def assign_work_type(dept: str) -> str:
    wt_map = WORK_TYPE_BY_DEPT[dept]
    return np.random.choice(list(wt_map.keys()), p=list(wt_map.values()))


def assign_salary_band(dept: str) -> str:
    sb_map = SALARY_BY_DEPT[dept]
    return np.random.choice(list(sb_map.keys()), p=list(sb_map.values()))


def clip_likert(arr: np.ndarray) -> np.ndarray:
    return np.clip(np.round(arr).astype(int), 1, 5)


def generate_climate_scores(
    departments: list,
    tenures: list,
    ages: list,
    is_outlier: np.ndarray,
) -> pd.DataFrame:
    n = len(departments)
    rng = np.random.default_rng(SEED)

    # Base scores — slightly positive skew (industry norm)
    base = rng.normal(loc=3.4, scale=0.7, size=(n, len(CLIMATE_COLS)))
    df = pd.DataFrame(base, columns=CLIMATE_COLS)

    dept_arr = np.array(departments)
    tenure_arr = np.array(tenures)
    age_arr = np.array(ages)

    # ── Bias 1: Maritime-Crew vs IT ──────────────────────────────────────────
    crew_mask = dept_arr == "Maritime-Crew"
    it_mask   = dept_arr == "IT"

    df.loc[crew_mask, "Work_Life_Balance"]  -= rng.uniform(1.0, 1.8, crew_mask.sum())
    df.loc[crew_mask, "Operational_Stress"] += rng.uniform(0.8, 1.5, crew_mask.sum())
    df.loc[crew_mask, "Job_Security"]       -= rng.uniform(0.3, 0.7, crew_mask.sum())
    df.loc[crew_mask, "Retention_Intent"]   -= rng.uniform(0.4, 0.9, crew_mask.sum())

    df.loc[it_mask,   "Work_Life_Balance"]  += rng.uniform(0.3, 0.7, it_mask.sum())
    df.loc[it_mask,   "Operational_Stress"] -= rng.uniform(0.5, 1.0, it_mask.sum())
    df.loc[it_mask,   "Tech_Adequacy"]      += rng.uniform(0.2, 0.6, it_mask.sum())

    # ── Bias 2: Mid-career crisis (2-4 years tenure) ─────────────────────────
    mid_mask = (tenure_arr >= 2) & (tenure_arr <= 4)
    df.loc[mid_mask, "Engagement"]        -= rng.uniform(0.5, 1.0, mid_mask.sum())
    df.loc[mid_mask, "Retention_Intent"]  -= rng.uniform(0.6, 1.2, mid_mask.sum())
    df.loc[mid_mask, "Career_Growth"]     -= rng.uniform(0.3, 0.8, mid_mask.sum())

    # ── Bias 3: Safety_Culture correlates with Managerial_Support ────────────
    managerial_noise = rng.normal(0, 0.4, n)
    df["Managerial_Support"] = (
        df["Managerial_Support"] * 0.4
        + df["Safety_Culture"] * 0.55
        + managerial_noise
    )

    # ── Bias 4: Younger employees — higher variance in Environmental_Pride ───
    young_mask = age_arr < 32
    df.loc[young_mask, "Environmental_Pride"] += rng.normal(
        0, 0.9, young_mask.sum()
    )

    # ── Bias 5: Tenure > 15 years → higher Job_Security & lower Career_Growth
    senior_mask = tenure_arr > 15
    df.loc[senior_mask, "Job_Security"]    += rng.uniform(0.3, 0.7, senior_mask.sum())
    df.loc[senior_mask, "Career_Growth"]   -= rng.uniform(0.2, 0.6, senior_mask.sum())
    df.loc[senior_mask, "Engagement"]      += rng.uniform(0.1, 0.4, senior_mask.sum())

    # ── Outliers: ~3% disgruntled, ~2% highly engaged ────────────────────────
    disgruntled_mask = is_outlier == -1
    promoted_mask    = is_outlier == 1

    df.loc[disgruntled_mask, ["Engagement", "Retention_Intent",
                               "Leadership_Trust", "Fair_Compensation",
                               "Recognition"]] -= rng.uniform(
        1.5, 2.5,
        (disgruntled_mask.sum(), 5)
    )
    df.loc[promoted_mask, ["Engagement", "Retention_Intent",
                            "Career_Growth", "Recognition",
                            "Managerial_Support"]] += rng.uniform(
        1.0, 2.0,
        (promoted_mask.sum(), 5)
    )

    # Clip all scores to [1, 5]
    for col in CLIMATE_COLS:
        df[col] = clip_likert(df[col].values)

    return df


def generate_chunk(start_idx: int, chunk_n: int, global_outlier_flags: np.ndarray) -> pd.DataFrame:
    end_idx = start_idx + chunk_n
    ids      = [f"MAR-{i:05d}" for i in range(start_idx + 1, end_idx + 1)]
    regions, countries = assign_region_country(chunk_n)

    departments = np.random.choice(DEPARTMENTS, size=chunk_n, p=DEPT_WEIGHTS).tolist()
    work_types  = [assign_work_type(d) for d in departments]
    genders     = np.random.choice(GENDER, size=chunk_n, p=GENDER_WEIGHTS).tolist()
    tenures     = np.round(np.random.uniform(0.5, 25, chunk_n), 1).tolist()
    salary_bands = [assign_salary_band(d) for d in departments]

    # Age: derived from tenure + random base (22–30 at hire)
    hire_ages = np.random.randint(22, 31, chunk_n)
    ages      = (np.array(hire_ages) + np.array(tenures)).tolist()

    outlier_flags = global_outlier_flags[start_idx:end_idx]
    climate_df    = generate_climate_scores(departments, tenures, ages, outlier_flags)

    df = pd.DataFrame({
        "Employee_ID":  ids,
        "Region":       regions,
        "Country":      countries,
        "Department":   departments,
        "Work_Type":    work_types,
        "Gender":       genders,
        "Age":          [round(a, 1) for a in ages],
        "Tenure_Years": tenures,
        "Salary_Band":  salary_bands,
    })

    df = pd.concat([df, climate_df.reset_index(drop=True)], axis=1)

    # ── Open_Comments (20% of rows) ──────────────────────────────────────────
    comment_cols = ["Engagement", "Retention_Intent", "Leadership_Trust",
                    "Work_Life_Balance", "Fair_Compensation"]
    avg_score = df[comment_cols].mean(axis=1)

    has_comment = np.random.random(chunk_n) < 0.20
    df["Open_Comments"] = ""
    df.loc[has_comment, "Open_Comments"] = [
        pick_comment(s) for s in avg_score[has_comment]
    ]

    return df


# ─── Main ─────────────────────────────────────────────────────────────────────

def main():
    print(f"Generating {N:,} records in chunks of {CHUNK_SIZE:,}...")

    # Pre-assign outlier flags across the full dataset
    outlier_flags = np.zeros(N, dtype=int)
    disgruntled_idx = np.random.choice(N, size=int(N * 0.03), replace=False)
    remaining       = np.setdiff1d(np.arange(N), disgruntled_idx)
    promoted_idx    = np.random.choice(remaining, size=int(N * 0.02), replace=False)
    outlier_flags[disgruntled_idx] = -1
    outlier_flags[promoted_idx]    = 1

    chunks = []
    for i, start in enumerate(range(0, N, CHUNK_SIZE)):
        chunk_n = min(CHUNK_SIZE, N - start)
        chunk   = generate_chunk(start, chunk_n, outlier_flags)
        chunks.append(chunk)
        print(f"  Chunk {i + 1}/{N // CHUNK_SIZE} done — rows {start + 1} to {start + chunk_n}")

    full_df = pd.concat(chunks, ignore_index=True)

    output_path = "workplace_climate_maritime_2026.csv"
    full_df.to_csv(output_path, index=False, encoding="utf-8")

    print(f"\nDataset saved → {output_path}")
    print(f"Shape: {full_df.shape[0]:,} rows x {full_df.shape[1]} columns")
    print("\nColumn list:")
    for col in full_df.columns:
        print(f"  {col}")
    print("\nSample stats (Climate Indicators):")
    print(full_df[CLIMATE_COLS].describe().round(2).to_string())
    print(f"\nOpen_Comments coverage: {(full_df['Open_Comments'] != '').sum():,} rows "
          f"({(full_df['Open_Comments'] != '').mean() * 100:.1f}%)")


if __name__ == "__main__":
    main()
