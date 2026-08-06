# LipidFlow — Phân tích cuộc họp 31/07/2026

**Người tham gia:** Helen (Speaker 3, code/demo app) · Dr. Xiaotao (Speaker 4, PI) · Dr. Chuchu (Speaker 5) · Sivani (Speaker 6, đối chiếu LipidSearch vs MS-DIAL)

---

## 1. Tóm tắt theo timeline

### 1.1 Home page & cấu trúc lipidAnalysis
- Home page dùng featureMSEA làm template tham chiếu - Dr. Xiaotao xác nhận đồng ý
- lipidAnalysis có **5 phần** - khớp đúng cấu trúc sidebar hiện tại (Data Import, Step 1-3, Save Results)
- Option A (New Analysis) + demo data đã demo trực tiếp, chạy được
- Format upload xác nhận: **mzXML**, cho phép upload POS/NEG cùng lúc

### 1.2 Sample metadata / group info - tạm hoãn, không phải bug
- Dr. Xiaotao: chỉ có tên file không đủ biết sample thuộc nhóm nào (control/case) - cần bảng metadata sample→group riêng
- Helen: sẽ thêm sau
- Dr. Chuchu phản biện ngay tại chỗ: việc này có thật sự cần lúc này không (liên quan normalization)?
- **Kết luận: chưa cần, hoãn lại có chủ đích** - không phải thiếu sót

### 1.3 Chiến lược lưu trữ & phân phối - RẤT QUAN TRỌNG
- Server chỉ có ~24-30GB, rất nhỏ
- Định hướng chính thức: **phân phối dưới dạng R package để user tự cài trên máy cá nhân**
- **Không** có kế hoạch cloud hosting cho dữ liệu lớn - chi phí quá cao (ước tính >$10,000/tháng ở Singapore)
- Server hiện tại chỉ dùng để demo/test vài mẫu nhỏ (2-3 sample), không phải nơi chạy phân tích thật quy mô lớn

### 1.4 Bảng Internal Standard - vấn đề đặt tên cột
- Dr. Chuchu: cột "UM" dễ nhầm với đơn vị micromet - nên đổi tên rõ nghĩa hơn (ví dụ "umol")
- Dr. Xiaotao: cần **validate format file ngay khi upload lần đầu**, báo lỗi rõ ràng nếu sai
- Help document phải mô tả rõ cấu trúc file mong đợi cho từng loại upload

### 1.5 Peak Picking - làm rõ khái niệm "raw data"
- Đổi nhãn "Raw data" → rõ nghĩa hơn, ví dụ "Mass spectrum raw data"
- Tranh luận dài Dr. Chuchu ↔ Dr. Xiaotao, xác nhận: **mzXML vẫn là raw data thật**, chỉ là định dạng chuẩn hoá từ định dạng riêng từng hãng máy (.raw Thermo, .d Agilent/Bruker)
- User bắt buộc tự convert sang mzXML trước (ProteoWizard/msConvert) - **help document phải nêu rõ** - đúng như README/User Guide hiện tại, xác nhận giữ nguyên
- "Upload from browser" vs "Existing path on server": Dr. Xiaotao thấy **gây khó hiểu**, đề xuất đơn giản hoá nhãn, chỉ cần nói "upload từ máy cá nhân"

### 1.6 MS2 data - phát hiện quan trọng: cần convert riêng, không dùng chung mzXML
- MS2 **đã có sẵn bên trong mzXML** (embedded), nhưng hệ thống **dùng MGF riêng cho Annotation** (tương thích tool khác)
- → User cần **2 lần convert từ raw gốc**: 1 lần ra mzXML (Peak Picking), 1 lần ra MGF (Annotation) - chưa được nói rõ trong tài liệu hiện tại
- App **hỗ trợ cả MGF lẫn MSP** cho input MS2 của user (không chỉ MGF)
- MGF lưu phổ dạng **centroid** (mỗi peak chỉ 1 điểm dữ liệu, không giữ hình Gaussian) - vì vậy file rất nhỏ, mất hình dạng peak thường **không ảnh hưởng** annotation

### 1.7 Database MS-DIAL → RDA - quyết định lớn nhất cuộc họp
- Chuỗi định dạng: MS-DIAL xuất `.msp` → cần convert sang `.rda` vì `metid` chỉ đọc RDA
- **Quyết định quan trọng nhất:** database nên **bundle thẳng vào app làm mặc định**, user **không cần tự upload** mỗi lần
- User vẫn có thể tự upload database khác nếu muốn (cá nhân hoá) - là lựa chọn phụ, không phải luồng chính
- Pipeline cần đúng **4 loại input file**: (1) mzXML raw, (2) MGF/MSP cho MS2, (3) database RDA, (4) bảng Internal Standard - khớp đúng Data Import hiện tại
- *Ghi chú tiến độ:* Helen có nhắc đã **tự thử convert database** rồi ("I use these libraries but I have to convert it in R") - chưa rõ xong hay đang dở, nên tự kiểm tra lại tình trạng

### 1.8 UX khi chạy job dài
- Dr. Xiaotao yêu cầu: khi job đang chạy, **toàn bộ giao diện nên "khoá"/tối màu**, ngăn user bấm lung tung chỗ khác
- Dẫn chứng: hành vi hiện tại của featureMSEA khi bấm Run

### 1.9 Bug thật, phát hiện trực tiếp khi test
- Dr. Xiaotao xem code output thật, phát hiện: kết quả Peak Picking **chỉ có `variable_info`** (feature ID, m/z, RT)
- **Thiếu hoàn toàn intensity theo từng sample** (`expression_data`)
- 1 peak table đúng: mỗi hàng = 1 feature, mỗi cột = 1 sample, giá trị = intensity
- Đây chính là dữ liệu Quantification cần dùng sau này - **bug thật, cần sửa sớm**

### 1.10 Batch effect khi xử lý nhiều sample
- Dr. Chuchu: RT và độ nhạy máy có thể trôi (drift) qua nhiều tháng chạy mẫu - khuyến nghị xử lý cùng 1 batch cùng lúc
- Dr. Xiaotao: TidyMass đã có "peak group"/alignment xử lý được ~40-50 sample/lần hợp lý (~10-20 phút/batch)
- Có sẵn hàm data normalization trong TidyMass, có thể tích hợp sau cho batch effect quy mô lớn hơn
- **Việc dời sang tương lai, chỉ cần ghi chú trong help document**

### 1.11 Đối chiếu LipidSearch vs MS-DIAL - phát hiện chênh lệch cần theo dõi
- Sivani: cùng 1 bộ dữ liệu, LipidSearch nhận diện ~113 lipid, MS-DIAL chỉ ~78, chỉ ~52 trùng nhau (chế độ âm) - chênh lệch lớn
- Nguyên nhân 1 phần: **hệ thống đặt tên/ID lipid khác nhau** giữa 2 database, khó so sánh trực tiếp
- Dr. Xiaotao đề xuất tìm 1 hệ thống đặt tên lipid chuẩn để quy đổi qua lại
- **Rủi ro khoa học thật, chưa có lời giải** - độ chính xác/đầy đủ của Annotation dựa trên MS-DIAL chưa chứng minh tương đương LipidSearch

### 1.12 Xem chất lượng peak - ghi nhận cho tương lai
- Dr. Chuchu đề xuất: cần cách xem lại hình dạng peak/EIC gốc để tự đánh giá annotation đúng/sai (giống cửa sổ xem peak của LipidSearch)
- Dr. Xiaotao xác nhận: tính năng **tương lai**, chưa làm

### 1.13 Database chuyên biệt cho membrane lipid - định hướng dài hạn
- Dr. Chuchu: lab quan tâm đặc biệt **membrane lipid** - đề xuất tương lai xây database **chuyên biệt cho membrane lipid** thay vì chỉ dùng MS-DIAL tổng quát
- Sivani có vẻ đồng tình, nhắc nhóm lipid cụ thể lab hay dùng (transcript lỗi đoạn này)
- Dr. Chuchu đánh giá: có thể là **"significant part"** của phần mềm sau này
- → Liên hệ trực tiếp mục 1.11: database nhỏ, curated đúng nhóm lipid lab quan tâm có thể giảm rủi ro chênh lệch đó cho use-case riêng của lab

### 1.14 MS-DIAL có nhiều database, không chỉ 1 file
- Sivani xác nhận MS-DIAL cung cấp **nhiều database khác nhau** (nội bộ + công khai tích hợp sẵn)
- Sivani thường chọn bản nhiều lipid nhất, nhưng còn nhiều lựa chọn khác
- → Khi tìm file MSP thật (việc đã giao 2 lượt trước): cần **chọn đúng bản** (nhiều lipid nhất hoặc phù hợp use-case), không tải bừa 1 file bất kỳ

---

## 2. Bảng hành động theo từng phần app

| Phần | Thay đổi | Tác động Frontend | Tác động Backend | Mức độ |
|---|---|---|---|---|
| Peak Picking | **Sửa bug: kết quả thiếu `expression_data`** | Bảng kết quả cần hiện thêm cột intensity/sample | Sửa `result_table` trong `peak_picking.R` để lấy cả `@expression_data`, không chỉ `@variable_info` | 🔴 Ngay - bug thật |
| Toàn app | Overlay "khoá" toàn màn hình khi job đang chạy | Thêm CSS/JS overlay (hoặc `waiter` package) | Không đổi backend, chỉ thêm trigger theo trạng thái job | 🟡 Nên làm sớm |
| Data Import | Đổi nhãn "Raw data" → "Mass spectrum raw data (.mzXML)" | Đổi text label, không đổi `inputId` | Không | 🟢 Nhỏ, dễ |
| Data Import | Đơn giản hoá nhãn Upload/Server path | Đổi text, cân nhắc gộp UX | Không | 🟡 Cần bàn thêm - đánh đổi với nhu cầu batch lớn |
| Data Import (Annotation) | Thêm `.msp` cho MS2 upload | Thêm accept type cho fileInput | Không đổi logic đọc file | 🟢 Nhỏ, dễ |
| Data Import (Annotation) | **Bundle database RDA mặc định vào package** | Bớt 1 bước thao tác cho user | Copy `.rda` vào `inst/db/`, sửa `.lfs_resolve_default_db()` | 🟡 Chờ có file MS-DIAL đã verify |
| IS table | Đổi tên cột "UM" → "umol" | Cập nhật help text | Không đổi code đọc file | 🟢 Chỉ cập nhật tài liệu |
| Data Import | Validate format file, báo lỗi cụ thể | Thêm thông báo lỗi rõ ràng | Thêm hàm kiểm tra schema từng loại file | 🟡 Nên làm, chưa có |
| Help document | Bổ sung: convert raw→mzXML, convert raw→MGF/MSP riêng, khuyến nghị batch cùng lúc | Không đổi code | Không đổi code | 🟢 Chỉ cập nhật User Guide |
| — | Sample metadata (group/condition) upload | — | — | ⚪ Đã hoãn có chủ đích |
| — | Xem lại peak shape/EIC để kiểm tra annotation | Cần khu vực hiển thị mới | Cần lưu EIC/peak shape data | ⚪ Tương lai - đã có sẵn chỗ ("Visualization Area") |
| — | Database chuyên biệt membrane lipid | Có thể cần thêm lựa chọn database | Cần tự curate database riêng - khối lượng lớn | ⚪ Tương lai, định hướng dài hạn |

---

## 3. Rủi ro / vấn đề mở, chưa có lời giải

- **Độ chính xác Annotation qua MS-DIAL chưa chứng minh tương đương LipidSearch** (113 vs 78 lipid, chỉ 52 trùng) - cần hệ thống đặt tên lipid chuẩn để so sánh công bằng, hiện chưa có
- **Chưa xác nhận MS-DIAL MSP parse đúng qua `metid`** (rủi ro đã nêu từ trước, cuộc họp không giải quyết thêm)
- Quyết định "bỏ server-path" của Dr. Xiaotao có thể mâu thuẫn lý do đã lập luận trước (dữ liệu vài trăm MB-GB không hợp lý upload qua trình duyệt) - cần làm rõ: bỏ hẳn tính năng hay chỉ đổi cách gọi tên
- Đoạn cuối cuộc họp (Speaker 7, dung lượng "~90-100K") - transcript quá lỗi để hiểu rõ, không đủ căn cứ đưa vào hành động - nêu ra để không bị coi là bỏ sót âm thầm

---

*Tổng hợp từ file ghi âm 31/07/2026, diễn giải lại theo ý logic (transcript lỗi nhận diện giọng nói nhiều chỗ - ví dụ "MTF"/"MSF" → MSP, "MGSF" → MGF, "Ms. style"/"MSDL" → MS-DIAL, "mad ID"/"meta ID" → metid).*
