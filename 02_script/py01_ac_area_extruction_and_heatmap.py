import pathlib
import os
import csv
import cv2
import numpy as np
from matplotlib.colors import LinearSegmentedColormap
import matplotlib.pyplot as plt


def extract_ac_area_old(img_file_name):
    img = cv2.imread(img_file_name)
    if img is None:
        return None, None, None, None, None

    # --- 旧コードそのまま ---
    gamma = 2.0
    gamma_cvt = np.zeros((256, 1), dtype=np.uint8)
    for i in range(256):
        gamma_cvt[i][0] = 255 * (float(i) / 255) ** (1.0 / gamma)
    img_gamma = cv2.LUT(img, gamma_cvt)

    img_yuv = cv2.cvtColor(img_gamma, cv2.COLOR_BGR2YUV)
    clahe = cv2.createCLAHE(clipLimit=2.0, tileGridSize=(8, 8))
    img_yuv[:, :, 0] = clahe.apply(img_yuv[:, :, 0])
    img = cv2.cvtColor(img_yuv, cv2.COLOR_YUV2BGR)

    img_Gblur = cv2.GaussianBlur(img, (15, 15), 0)
    gray_Gblur = cv2.cvtColor(img_Gblur, cv2.COLOR_BGR2GRAY)

    # 魚体抽出
    _, thresh_img2 = cv2.threshold(gray_Gblur, 200, 255, cv2.THRESH_BINARY_INV)
    contours2, hierarchy2 = cv2.findContours(
        thresh_img2, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE
    )
    if len(contours2) == 0:
        return None, None, None, None, None
    max_cnt2 = max(contours2, key=lambda x: cv2.contourArea(x))
    out2 = np.zeros_like(thresh_img2)
    fish_mask = cv2.drawContours(out2, [max_cnt2], -1, color=200, thickness=-1)

    # 赤色抽出
    hsv_img = cv2.cvtColor(img_Gblur, cv2.COLOR_BGR2HSV)
    rgb_img = cv2.cvtColor(img_Gblur, cv2.COLOR_BGR2RGB)
    mask = cv2.inRange(hsv_img, (0, 90, 140), (40, 255, 250))
    result = cv2.bitwise_and(rgb_img, rgb_img, mask=mask)
    result_gray = cv2.cvtColor(result, cv2.COLOR_RGB2GRAY)
    _, thresh_img = cv2.threshold(result_gray, 50, 255, cv2.THRESH_BINARY)
    contours, hierarchy = cv2.findContours(
        thresh_img, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE
    )
    if len(contours) == 0:
        ac_mask = np.zeros_like(thresh_img)
        fish_Area = cv2.countNonZero(fish_mask)
        ac_Area = 0
        ac_ratio = ac_Area / fish_Area if fish_Area > 0 else None
        return fish_mask, ac_mask, fish_Area, ac_Area, ac_ratio

    max_cnt = max(contours, key=lambda x: cv2.contourArea(x))
    out = np.zeros_like(thresh_img)
    ac_mask = cv2.drawContours(out, [max_cnt], -1, color=255, thickness=-1)

    fish_Area = cv2.countNonZero(fish_mask)
    ac_Area = cv2.countNonZero(ac_mask)
    ac_ratio = ac_Area / fish_Area if fish_Area > 0 else None

    return fish_mask, ac_mask, fish_Area, ac_Area, ac_ratio

def process_ac_mask_with_pca(fish_mask, ac_mask, output_size=(256, 64)):
    # 輪郭取得（魚体）
    contours, _ = cv2.findContours(fish_mask, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    if not contours:
        return None
    contour = max(contours, key=cv2.contourArea)

    data_pts = contour.reshape(-1, 2).astype(np.float32)
    mean, eigenvectors = cv2.PCACompute(data_pts, mean=None)
    angle = np.arctan2(eigenvectors[0, 1], eigenvectors[0, 0])
    cos_a, sin_a = np.cos(-angle), np.sin(-angle)
    R = np.array([[cos_a, -sin_a], [sin_a, cos_a]], dtype=np.float32)

    centered_pts = data_pts - mean
    rotated_pts = centered_pts @ R.T
    min_xy = rotated_pts.min(axis=0)
    max_xy = rotated_pts.max(axis=0)
    size = max_xy - min_xy
    scale_x = output_size[0] / size[0]
    scale_y = output_size[1] / size[1]
    scale = min(scale_x, scale_y)

    translation = (-mean @ R.T) * scale - min_xy * scale
    M = np.hstack([R * scale, translation.reshape(2, 1)])

    aligned_ac = cv2.warpAffine(ac_mask, M, dsize=(output_size[0], output_size[1]))
    aligned_fish = cv2.warpAffine(fish_mask, M, dsize=(output_size[0], output_size[1]))

    return aligned_ac, aligned_fish

def resample_contour(contour, num_points=100):
    contour = contour[:, 0, :]  # (N, 2)
    contour = np.vstack([contour, contour[0]])  # 閉曲線化

    distances = np.cumsum(np.sqrt(np.sum(np.diff(contour, axis=0)**2, axis=1)))
    distances = np.insert(distances, 0, 0)

    interp_x = np.interp(np.linspace(0, distances[-1], num_points), distances, contour[:, 0])
    interp_y = np.interp(np.linspace(0, distances[-1], num_points), distances, contour[:, 1])

    return np.stack([interp_x, interp_y], axis=1)


def create_ac_heatmap_with_pca(image_paths, output_size=(256, 64)):
    heatmap = np.zeros((output_size[1], output_size[0]), dtype=np.float32)
    aligned_contours = []
    count = 0

    for path in image_paths:
        fish_mask, ac_mask, fish_Area, ac_Area, ac_ratio = extract_ac_area_old(str(path))
        if fish_mask is None or ac_mask is None:
            continue

        result = process_ac_mask_with_pca(fish_mask, ac_mask, output_size=output_size)
        if result is None:
            continue
        aligned_ac, aligned_fish = result

        if aligned_ac.shape != heatmap.shape:
            print(f"サイズ不一致: {path} -> {aligned_ac.shape}, スキップ")
            continue

        heatmap += aligned_ac / 255.0
        count += 1

        # 平均輪郭用に魚体輪郭を保存
        contours, _ = cv2.findContours(aligned_fish, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        if contours:
            contour = max(contours, key=cv2.contourArea)
            contour_resampled = resample_contour(contour, num_points=100)
            aligned_contours.append(contour_resampled)


    if count > 0:
        heatmap /= count
    else:
        print("有効な画像がありませんでした。")

    avg_contour = None
    if len(aligned_contours) > 0:
        contour_array = np.stack(aligned_contours)
        avg_contour = np.mean(contour_array, axis=0)

    return heatmap, avg_contour

def visualize_ac_heatmap_with_average_contour(
        heatmap,
        avg_contour,
        output_path_hot,
        output_path_reds):

    # 論文前の確認用（hot）
    plt.figure(figsize=(10, 4))
    im = plt.imshow(
        heatmap,
        cmap='hot',
        interpolation='nearest',
        vmin=0,
        vmax=0.8
    )
    plt.title("AC Heatmap + Mean Fish Contour")
    plt.colorbar(im, label='Frequency')
    plt.axis('off')
    if avg_contour is not None:
        plt.plot(avg_contour[:, 0], avg_contour[:, 1],
                 color='cyan', linewidth=2)
    plt.savefig(output_path_hot, bbox_inches='tight')
    plt.close()
    print(f"✅ 保存: {output_path_hot}")

    # 論文用：白背景＋赤頻度＋黒輪郭
    custom_red = LinearSegmentedColormap.from_list(
        "custom_red",
        ["#ffffff", "#ffb3b3", "#ff6666", "#ff0000"]
    )
    plt.figure(figsize=(10, 4), facecolor="white")
    im = plt.imshow(
        heatmap,
        cmap=custom_red,
        interpolation="nearest",
        vmin=0,
        vmax=0.8
    )
    plt.colorbar(im, label="Frequency")
    if avg_contour is not None:
        plt.plot(avg_contour[:, 0], avg_contour[:, 1],
                 color="black", linewidth=2)
    plt.title("AC Heatmap + Mean Fish Contour")
    plt.axis("off")
    plt.savefig(
        output_path_reds,
        bbox_inches="tight",
        facecolor="white"
    )
    plt.close()
    print(f"✅ 保存: {output_path_reds}")



# ここに extract_ac_area_old, create_heatmap_with_pca,
# extract_standardized_contour, resample_contour,
# visualize_heatmap_with_average_contour を定義しておく

# ==================================================
# 種ごとのパイプライン実行
# ==================================================
def run_pipeline_for_species(species):
    print(f"=== {species} ===")

    # ★ パス構造を変更
    project_root = pathlib.Path(__file__).resolve().parent.parent
    input_dir = project_root / f"01_data/abdominal_images/ventral_images_{species}/"

    output_dir = project_root / pathlib.Path(f"11_abdominal_data_processed/{species}/Fig2top/")
    mask_dir = project_root / pathlib.Path(f"11_abdominal_data_processed/{species}/ac_masks/")

    os.makedirs(output_dir, exist_ok=True)
    os.makedirs(mask_dir, exist_ok=True)

    # CSV の保存場所も data_processed_abdomen に変更
    csv_path = output_dir / f"AC_measurements_{species}.csv"

    image_paths = list(input_dir.glob("**/*.jpg"))
    print("画像枚数:", len(image_paths))

    # CSV 初期化
    with open(csv_path, "w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["filename", "fish_area", "ac_area", "ac_ratio"])

    # AC 抽出
    for p in image_paths:
        fish_mask, ac_mask, fish_Area, ac_Area, ac_ratio = extract_ac_area_old(str(p))
        if fish_mask is None:
            print("⚠ AC 抽出失敗:", p)
            continue

        cv2.imwrite(str(mask_dir / p.name), ac_mask)

        with open(csv_path, "a", newline="") as f:
            writer = csv.writer(f)
            writer.writerow([p.name, fish_Area, ac_Area, ac_ratio])

    # ★ AC ヒートマップ + 平均輪郭（統合版）
    heatmap, avg_contour = create_ac_heatmap_with_pca(
        [str(p) for p in image_paths],
        output_size=(256, 64),
    )

    # ★ 図の保存パスとファイル名を Fig2top に変更
    output_path_hot = str(output_dir / "Fig2top_sub.png")
    output_path_reds = str(output_dir / "Fig2top.png")

    visualize_ac_heatmap_with_average_contour(
        heatmap,
        avg_contour,
        output_path_hot,
        output_path_reds,
    )

    print("✅ CSV:", csv_path)
    print("✅ 図:", output_path_hot)
    print("✅ 図:", output_path_reds)


# ==================================================
# DV / WSC を順番に実行
# ==================================================
if __name__ == "__main__":
    for species in ["DV", "WSC"]:
        run_pipeline_for_species(species)
