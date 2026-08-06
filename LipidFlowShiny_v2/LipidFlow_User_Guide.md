# LipidFlow — User Guide

*Style theo đúng cấu trúc [Overview of lipidflow](https://www.jaspershenlab.com/lipidflow/articles/overview_of_lipidflow.html) - liệt kê "Data organization" trước, rồi mới tới từng bước.*

---

# Data organization

Trước khi bắt đầu, LipidFlow cần **4 loại dữ liệu**, tất cả upload trong **Data Import**:

1. **Mass spectrum raw data** (mzXML) - cho Peak Picking
2. **MS2 spectral data** (MGF hoặc MSP) - cho Lipid Annotation
3. **Spectral database** (.rda) - cho Lipid Annotation (đã có sẵn database mặc định, không bắt buộc tự upload)
4. **Internal standard information** (.xlsx) - cho Quantification

---

## 1. Convert raw data to mzXML

Dữ liệu raw từ máy khối phổ (định dạng riêng từng hãng: `.raw` của Thermo, `.d` của Agilent/Bruker...) cần convert sang **mzXML** trước, dùng [ProteoWizard (msConvert)](http://proteowizard.sourceforge.net/).

**Lưu ý quan trọng:** mzXML **vẫn là raw data thật** - chỉ là định dạng đã chuẩn hoá, không phải dữ liệu đã qua xử lý/lọc gì. LipidFlow **không** làm được bước convert này - phải tự làm trước bằng ProteoWizard.

Tổ chức file theo group ngay từ tên file trước khi upload - ví dụ nhóm `D25` (2 lần lặp) và `M19` (2 lần lặp):
```
D25_1.mzXML
D25_2.mzXML
M19_1.mzXML
M19_2.mzXML
```
LipidFlow tự đọc group từ tên file (bỏ số thứ tự cuối) - không cần bảng metadata riêng cho bước này.

---

## 2. Convert raw data to MS2 (MGF hoặc MSP)

**Đây là bước convert RIÊNG BIỆT**, khác với bước convert mzXML ở trên - dù MS2 vốn đã có sẵn trong file raw gốc, LipidFlow cần trích riêng ra dạng MGF hoặc MSP để dùng cho Annotation. Cũng dùng ProteoWizard, xuất ra định dạng MGF (hoặc MSP) thay vì mzXML.

LipidFlow hỗ trợ **cả 2 định dạng** MGF và MSP cho bước này.

---

## 3. Spectral database

LipidFlow dùng thư viện phổ MS2 công khai từ **MS-DIAL**, đã được convert sang định dạng `.rda` (định dạng package `metid` yêu cầu) và **có sẵn mặc định trong app** - phần lớn trường hợp không cần tự upload gì ở đây.

Nếu muốn dùng database khác (cá nhân hoá, hoặc database riêng của bạn), vẫn có thể tự upload trong Data Import.

---

## 4. Internal standard information

Dữ liệu cần dạng **xlsx**, đúng 5 cột theo thứ tự sau:

| Cột | Ý nghĩa |
|---|---|
| `name` | Tên internal standard |
| `exact.mass` | Khối lượng chính xác |
| `formula` | Công thức hoá học |
| `ug_ml` | Nồng độ (µg/mL) |
| `um` | Nồng độ (µmol) - *lưu ý: dù cột tên là "um", đơn vị thật là µmol, không phải µm (đơn vị chiều dài) - tránh nhầm lẫn* |

LipidFlow tự kiểm tra đủ 5 cột này khi bạn upload - báo lỗi ngay nếu thiếu hoặc sai tên cột.

---

# Workflow - 5 bước trong lipidAnalysis

Mỗi bước tự động nhận kết quả từ bước trước - không cần upload lại dữ liệu đã dùng.

---

## Bước 1: Data Import

Nơi duy nhất cần upload/nhập file cho cả pipeline - xem chi tiết 4 loại dữ liệu ở phần Data organization phía trên.

- **Peak Picking data**: chọn Option A (phân tích mới - upload file hoặc dùng demo data) hoặc Option B (nạp lại kết quả đã chạy trước)
- **Lipid Annotation data**: file MS2 (MGF/MSP) + database (.rda, có mặc định sẵn)
- **Quantification data**: bảng Internal Standard

---

## Bước 2: Peak Picking

Bấm **Run** ở Step 1. Tham số kỹ thuật (ppm, peak width...) nằm trong **Advanced settings**, mặc định phù hợp cho hầu hết trường hợp.

Kết quả gồm 2 phần:
- **Peak table**: bảng đầy đủ - mỗi hàng 1 peak (m/z, RT), mỗi cột 1 sample là giá trị intensity. Tải về bằng nút **Download CSV**.
- **QC plots**: biểu đồ TIC/BPC (kiểm tra chất lượng lần chạy máy) và RT correction (kiểm tra alignment giữa các sample) - không bắt buộc xem, nhưng nên kiểm tra nếu chuẩn bị dùng kết quả để công bố.

---

## Bước 3: Lipid Annotation

Tự động nhận feature từ Bước 2. Chọn loại cột sắc ký (`rp`/`hilic`), bấm **Annotate**.

---

## Bước 4: Quantification

Tự động nhận kết quả đã annotate từ Bước 3. Chọn internal standard muốn dùng, bấm **Quantify**.

---

## Bước 5: Save Results

Tải kết quả mới nhất (**Save Result**) hoặc toàn bộ dữ liệu đã tạo ra trong phiên làm việc (**Save All Data**).

---

# Khuyến nghị khi xử lý dữ liệu lớn

Nếu có nhiều mẫu chạy trải dài qua nhiều tháng, thời gian lưu giữ (RT) và độ nhạy máy có thể trôi (drift) giữa các đợt chạy. Khuyến nghị xử lý **cùng 1 batch dữ liệu cùng lúc** thay vì gộp nhiều đợt chạy cách xa nhau vào 1 lần phân tích.

---

# Troubleshooting

| Triệu chứng | Nguyên nhân | Cách xử lý |
|---|---|---|
| Status báo Error sau khi Run | Thiếu file bắt buộc | Xem log panel - báo rõ đang thiếu gì |
| Upload IS table báo lỗi | Sai tên cột hoặc thiếu cột | Đối chiếu đúng 5 cột ở mục "Internal standard information" |
| Bước 2/3 báo "No data yet" | Bước trước chưa hoàn thành | Quay lại hoàn thành bước trước |
| QC plots không hiện | Đang dùng Option B (load lại kết quả cũ) không có sẵn file PDF | Bình thường - chỉ Option A (chạy mới) mới có |
