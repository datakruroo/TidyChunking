# TidyChunking Demo Script
# ========================

library(TidyChunking)

# Sample Thai markdown content about data literacy
markdown_text <- "
# บทที่ 1: ความรู้เบื้องต้นเกี่ยวกับ Data Literacy

## 1.1 แนวคิดและความหมาย

Data literacy หรือ ความรู้ด้านข้อมูล เป็นความสามารถในการอ่าน เข้าใจ วิเคราะห์ และใช้ข้อมูลอย่างมีประสิทธิภาพ ในยุคดิจิทัลปัจจุบัน ทักษะนี้ถือเป็นสิ่งจำเป็นสำหรับบุคคลในทุกสาขาอาชีพ

ความสำคัญของ data literacy ในโลกสมัยใหม่สามารถสรุปได้ดังนี้:
- ช่วยในการตัดสินใจที่มีฐานข้อมูลรองรับ
- เพิ่มขีดความสามารถในการแข่งขันของบุคคลและองค์กร
- ลดความเสี่ยงจากการตัดสินใจที่ผิดพลาด

## 1.2 องค์ประกอบของ Data Literacy

### 1.2.1 การอ่านและเข้าใจข้อมูล

การอ่านข้อมูลเป็นทักษะพื้นฐานที่สำคัญ ประกอบด้วย:
- การแปลความหมายจากตัวเลขและกราฟ
- การระบุแนวโน้มและรูปแบบ
- การเปรียบเทียบข้อมูลจากหลายแหล่ง

### 1.2.2 การวิเคราะห์ข้อมูล

ทักษะการวิเคราะห์ช่วยให้เราสามารถ:
- ค้นหาความสัมพันธ์ระหว่างตัวแปร
- ทำนายแนวโน้มในอนาคต
- สร้างข้อสรุปที่มีความหมาย

# บทที่ 2: เครื่องมือและเทคนิค

## 2.1 เครื่องมือสำหรับการวิเคราะห์ข้อมูล

### 2.1.1 Microsoft Excel

Excel เป็นเครื่องมือพื้นฐานที่ใช้กันอย่างแพร่หลาย มีความสามารถใน:
- การจัดการและจัดเรียงข้อมูล
- การสร้างกราฟและแผนภูมิ
- การคำนวณทางสถิติพื้นฐาน

### 2.1.2 R Programming

R เป็นภาษาโปรแกรมที่เฉพาะสำหรับการวิเคราะห์ข้อมูล มีจุดเด่นใน:
- การจัดการข้อมูลขนาดใหญ่
- การสร้างกราฟที่สวยงามและมีคุณภาพสูง
- การวิเคราะห์ทางสถิติขั้นสูง

## 2.2 ตัวอย่างการประยุกต์ใช้

### การวิเคราะห์ยอดขาย

บริษัท ABC ใช้ data literacy ในการวิเคราะห์ยอดขายประจำเดือน พบว่า:
- ยอดขายเพิ่มขึ้น 15% ในช่วงไตรมาสที่ 3
- สินค้าประเภท X มียอดขายสูงสุด
- ลูกค้าในภาคเหนือมีการตอบสนองดีที่สุด

ข้อมูลเหล่านี้ช่วยให้บริษัทสามารถวางแผนการตลาดและการผลิตได้อย่างมีประสิทธิภาพ

# บทที่ 3: การพัฒนาทักษะ

## 3.1 แนวทางการเรียนรู้

### 3.1.1 การศึกษาพื้นฐาน

เริ่มต้นด้วยการศึกษาแนวคิดพื้นฐาน:
- สถิติเบื้องต้น
- การอ่านและการตีความกราฟ
- หลักการของ database

### 3.1.2 การฝึกปฏิบัติ

ลงมือปฏิบัติจริงด้วยข้อมูลตัวอย่าง:
- ทำโครงการเล็ก ๆ ด้วยข้อมูลส่วนตัว
- เข้าร่วมการแข่งขัน data science
- สร้าง portfolio ของผลงาน

# References และบรรณานุกรม

- Smith, J. (2023). Data Literacy in the Digital Age. Academic Press.
- Johnson, M. (2022). Understanding Data: A Practical Guide. Tech Publications.
- การวิจัยและพัฒนา. กรมสถิติแห่งชาติ (2023). แนวทางการพัฒนา Data Literacy ในประเทศไทย.
"

# Step 1: Chunk the markdown text
cat("=== Step 1: Chunking Markdown Text ===\n")
chunks <- chunk_for_keyword_extraction(
  markdown_text, 
  max_words = 500,  # ใช้ขนาดเล็กกว่าเพื่อให้เห็นผลชัดเจน
  min_words = 50
)

cat("Total chunks created:", nrow(chunks), "\n\n")

# Step 2: Preview all chunks
cat("=== Step 2: Preview All Chunks ===\n")
preview_chunks(chunks)

# Step 3: Filter chunks for keyword extraction
cat("\n=== Step 3: Filter for Keyword Extraction ===\n")
keyword_chunks <- filter_chunks_for_keywords(chunks)

cat("Chunks after filtering:", nrow(keyword_chunks), "\n\n")

# Step 4: Preview filtered chunks
cat("=== Step 4: Preview Filtered Chunks ===\n")
preview_chunks(keyword_chunks)

# Step 5: Show detailed example of first chunk
cat("\n=== Step 5: Example Chunk Detail ===\n")
if (nrow(keyword_chunks) > 0) {
  example_chunk <- keyword_chunks[1, ]
  cat("Chunk ID:", example_chunk$chunk_id, "\n")
  cat("Hierarchy:", example_chunk$hierarchy, "\n")
  cat("Word Count:", example_chunk$word_count, "\n")
  cat("Content Type:", example_chunk$content_type, "\n")
  cat("Content Preview:\n")
  cat(substr(example_chunk$chunk_text, 1, 200), "...\n")
}

cat("\n=== Demo Complete! ===\n")
cat("To extract competencies with AI, install 'tidyllm' and 'jsonlite' packages\n")
cat("and use: extract_competencies_tidyllm(keyword_chunks)\n")