# TidyChunking <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-che### Extracted Teacher Competencies

```r
# A tibble: 15 × 6
   term                     category  importance definition                                source_chunk source_hierarchy              
   <chr>                    <chr>     <chr>      <chr>                                <chr>        <chr>                        
 1 formative assessment     skill     high       "วิเคราะห์ข้อมูลการประเมินเพื่อปรับการสอน"     2.1          "การประเมินผลในชั้นเรียน"        
 2 student data analysis    knowledge high       "ความรู้การวิเคราะห์ข้อมูลนักเรียน"          4.1          "การใช้ข้อมูลเพื่อการสอน"     
 3 data-driven instruction  practice  high       "การสอนที่อิงข้อมูลเชิงประจักษ์"           4.1          "วิธีการสอนสมัยใหม่"     
```//github.com/datakruroo/TidyChunking/workflows/R-CMD-check/badge.svg)](https://github.com/datakruroo/TidyChunking/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/TidyChunking)](https://CRAN.R-project.org/package=TidyChunking)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**TidyChunking** เป็น R package สำหรับการแบ่ง (chunking) เอกสาร Markdown อย่างชาญฉลาด เพื่อเตรียมข้อมูลสำหรับการวิเคราะห์ข้อความและการสกัดคำสำคัญ

## ✨ คุณสมบัติหลัก

- 🎯 **Structure-Aware Chunking**: เคารพโครงสร้างลำดับชั้น markdown headings
- 📏 **Optimal Chunk Sizes**: ควบคุมขนาด chunk ให้เหมาะสมกับการประมวลผล
- 🏷️ **Content Classification**: จำแนกประเภทเนื้อหาอัตโนมัติ (main content, examples, metadata)
- 🔍 **Keyword Extraction Ready**: ออกแบบมาเฉพาะสำหรับการสกัดคำสำคัญ
- 🤖 **Teacher-Focused AI**: สกัด competencies เฉพาะสำหรับบัณฑิตครูในการทำ data-driven classroom

## 📦 การติดตั้ง

```r
# จาก GitHub
# install.packages("devtools")
devtools::install_github("datakruroo/TidyChunking")

# สำหรับ AI competency extraction (optional)
install.packages(c("tidyllm", "jsonlite"))
```

## 🚀 การใช้งานพื้นฐาน

### 1. Basic Chunking

```r
library(TidyChunking)

# ตัวอย่างข้อความ markdown
markdown_text <- "
# บทที่ 1: ความรู้เบื้องต้น

## 1.1 แนวคิดพื้นฐาน
Data literacy เป็นความสามารถในการอ่าน เข้าใจ และใช้งานข้อมูลอย่างมีประสิทธิภาพ...

## 1.2 ตัวอย่างการใช้งาน
ในองค์กรสมัยใหม่ การตัดสินใจที่ดีต้องอาศัยข้อมูลเป็นหลัก...

# บทที่ 2: เครื่องมือและเทคนิค

## 2.1 การใช้ R
R เป็นภาษาโปรแกรมที่เหมาะสำหรับการวิเคราะห์ข้อมูล...
"

# แบ่ง chunks
chunks <- chunk_for_keyword_extraction(
  markdown_text, 
  max_words = 800,
  min_words = 100
)

# ดูผลลัพธ์
preview_chunks(chunks)
```

### 2. กรองข้อมูลสำหรับการสกัดคำสำคัญ

```r
# กรองเฉพาะ chunks ที่เหมาะสมสำหรับการสกัดคำสำคัญ
keyword_chunks <- filter_chunks_for_keywords(chunks)

# ดูข้อมูลที่กรองแล้ว
preview_chunks(keyword_chunks)
```

### 3. การสกัด Competencies ด้วย AI (ต้องมี tidyllm)

```r
# ขั้นตอนที่ 1: ตั้ง API key (ทำครั้งเดียว)
# เปิดไฟล์ .Renviron
usethis::edit_r_environ()

# เพิ่มบรรทัดนี้ในไฟล์ .Renviron
# OPENAI_API_KEY="your-api-key-here"

# ขั้นตอนที่ 2: Restart R session
# ขั้นตอนที่ 3: ติดตั้ง packages ที่จำเป็น
install.packages(c("tidyllm", "jsonlite"))

# ขั้นตอนที่ 4: ตรวจสอบการตั้งค่า (แนะนำ)
check_openai_setup()  # ช่วย debug ปัญหา API

# ขั้นตอนที่ 5: สกัด competencies สำหรับครู
if (require(tidyllm) && require(jsonlite)) {
  
  # สกัด competencies เฉพาะสำหรับบัณฑิตครูในการทำ data-driven classroom
  teacher_competencies <- extract_competencies_tidyllm(
    keyword_chunks, 
    max_per_chunk = 10
  )
  
  # ขั้นตอนที่ 6: ตรวจสอบคุณภาพผลลัพธ์ (แนะนำ)
  validated_competencies <- validate_competencies(teacher_competencies, chunks)
  
  # ดูผลลัพธ์
  head(validated_competencies)
  table(validated_competencies$category)
  table(validated_competencies$confidence > 0.7)  # ดูความมั่นใจ
}
```

### การปรับแต่ง extract_competencies_tidyllm

Function นี้สามารถปรับแต่งได้ 3 ประการหลัก:

#### 1. เลือก Model

```r
# ใช้ GPT-4 แทน default gpt-4o-mini (ถ้ามี access)
teacher_comp_gpt4 <- extract_competencies_tidyllm(
  keyword_chunks,
  model = "gpt-4"
)

# ใช้ GPT-3.5-turbo (ถูกกว่า)
teacher_comp_gpt35 <- extract_competencies_tidyllm(
  keyword_chunks,
  model = "gpt-3.5-turbo"
)
```

#### 2. Custom Prompt Function

```r
# สร้าง prompt function สำหรับ business skills
business_prompt <- function(n_comp, hierarchy, text) {
  paste0(
    "Extract ", n_comp, " BUSINESS SKILLS from this text.\n",
    "Categories: technical, analytical, communication, leadership\n",
    "JSON format: [{\"skill\": \"name\", \"type\": \"technical\", \"level\": \"advanced\"}]\n",
    "Text: ", text
  )
}

# ใช้ custom prompt
business_skills <- extract_competencies_tidyllm(
  keyword_chunks,
  custom_prompt = business_prompt
)
```

#### 3. Custom Schema

```r
# สร้าง schema สำหรับ business skills
business_schema <- tidyllm::tidyllm_schema(
  name = "business_extraction",
  competencies = tidyllm::field_object(
    .vector = TRUE,
    skill = tidyllm::field_chr(.description = "Business skill name"),
    type = tidyllm::field_fct(.levels = c("technical", "analytical", "communication", "leadership")),
    level = tidyllm::field_fct(.levels = c("beginner", "intermediate", "advanced"))
  )
)

# ใช้ custom prompt และ schema ร่วมกัน
business_skills <- extract_competencies_tidyllm(
  keyword_chunks,
  custom_prompt = business_prompt,
  custom_schema = business_schema
)
```

#### ตัวอย่างครบชุดสำหรับ Healthcare

```r
# Healthcare prompt
healthcare_prompt <- function(n_comp, hier, text) {
  paste0(
    "Extract ", n_comp, " HEALTHCARE COMPETENCIES.\n",
    "Categories: clinical, technical, communication, safety\n",
    "Priority: critical, important, useful\n",
    "JSON: [{\"competency\": \"name\", \"type\": \"clinical\", \"priority\": \"critical\"}]\n",
    "Text: ", text
  )
}

# Healthcare schema  
healthcare_schema <- tidyllm::tidyllm_schema(
  name = "healthcare",
  competencies = tidyllm::field_object(
    .vector = TRUE,
    competency = tidyllm::field_chr(.description = "Healthcare competency"),
    type = tidyllm::field_fct(.levels = c("clinical", "technical", "communication", "safety")),
    priority = tidyllm::field_fct(.levels = c("critical", "important", "useful"))
  )
)

# Extract healthcare competencies
healthcare_comp <- extract_competencies_tidyllm(
  keyword_chunks,
  model = "gpt-4o-mini",
  custom_prompt = healthcare_prompt,
  custom_schema = healthcare_schema
)
```

> **📁 ดูตัวอย่างเพิ่มเติม:** `inst/examples/demo_advanced.R`
```

## 📊 ตัวอย่างผลลัพธ์

### Chunk Structure

```r
# A tibble: 4 × 9
  chunk_id chunk_text           word_count heading     level parent_h1 parent_h2 hierarchy            content_type
  <chr>    <chr>                     <int> <chr>       <int> <chr>     <chr>     <chr>                <chr>       
1 1.1      "# บทที่ 1: ความรู้เบื้องต้น…     156 "ความรู้เบื้องต้น"      1 "ความรู้เบื้องต้น" NA        "ความรู้เบื้องต้น"           main_content
2 2.1      "## 1.1 แนวคิดพื้นฐาน\nDa…     234 "แนวคิดพื้นฐาน"         2 "ความรู้เบื้องต้น" "แนวคิดพื้นฐาน" "ความรู้เบื้องต้น > แนวคิดพื้นฐาน" main_content
3 3.1      "## 1.2 ตัวอย่างการใช้งาน\n…     189 "ตัวอย่างการใช้งาน"        2 "ความรู้เบื้องต้น" "ตัวอย่างการใช้งาน" "ความรู้เบื้องต้น > ตัวอย่างการใช้งาน" example    
4 4.1      "## 2.1 การใช้ R\nR เป็นภา…     267 "การใช้ R"            2 "เครื่องมือและเทคนิค" "การใช้ R" "เครื่องมือและเทคนิค > การใช้ R" main_content
```

### Extracted Competencies

```r
# A tibble: 15 × 6
   term                category  importance definition                    source_chunk source_hierarchy              
   <chr>               <chr>     <chr>      <chr>                        <chr>        <chr>                        
 1 data literacy       knowledge high       "ความสามารถในการอ่าน เข้าใจ และใช้งานข้อมูล" 2.1          "ความรู้เบื้องต้น > แนวคิดพื้นฐาน"        
 2 data visualization  skill     high       "การสร้างกราफิกเพื่อนำเสนอข้อมูล"      4.1          "เครื่องมือและเทคนิค > การใช้ R"     
 3 statistical analysis skill     medium     "การวิเคราะห์ข้อมูลเชิงสถิติ"         4.1          "เครื่องมือและเทคนิค > การใช้ R"     
```

## 🎛️ การปรับแต่ง

### พารามิเตอร์หลัก

- **`max_words`** (800): จำนวนคำสูงสุดต่อ chunk
- **`min_words`** (100): จำนวนคำต่ำสุดที่ถือว่าเป็น main content  
- **`max_per_chunk`** (15): จำนวน competencies สูงสุดที่สกัดต่อ chunk

### Content Types

- **`main_content`**: เนื้อหาหลักที่เหมาะสำหรับการวิเคราะห์
- **`example`**: ตัวอย่างและกรณีศึกษา
- **`metadata`**: ข้อมูลอ้างอิง, บรรณานุกรม, อภิธานศัพท์
- **`table_only`**: ตารางข้อมูลเปล่า ๆ
- **`other`**: เนื้อหาอื่น ๆ ที่ไม่เข้าหมวดหมู่

## 📋 ข้อกำหนด

### พื้นฐาน
- R >= 4.0.0
- dplyr, tidyr, stringr, purrr, tibble, magrittr

### สำหรับ AI Features (ไม่จำเป็น)
- tidyllm
- jsonlite  
- OpenAI API key

## 🎯 Use Cases

1. **การเตรียมข้อมูลสำหรับ RAG systems**
2. **การสกัดคำสำคัญจากเอกสารขนาดใหญ่**
3. **การวิเคราะห์เนื้อหาเชิงโครงสร้าง**
4. **การสร้าง competency frameworks**
5. **การจัดหมวดหมู่เนื้อหาอัตโนมัติ**

## 🔧 Troubleshooting

### ปัญหา API ที่พบบ่อย

```r
# ตรวจสอบการตั้งค่า API
check_openai_setup()
```

**หา API Key ไม่เจอ:**
- ตรวจสอบไฟล์ `.Renviron` มี `OPENAI_API_KEY="..."` หรือไม่
- Restart R session หลังแก้ไข `.Renviron`

**Quota ไม่พอ (แม้เติมเงินแล้ว):**
- เช็ค [OpenAI Billing](https://platform.openai.com/account/billing)
- ดูว่า payment method ใช้งานได้ไหม
- ลองสร้าง API key ใหม่

**Rate Limit:**
- รอสักครู่แล้วลองใหม่
- ลดจำนวน chunks ที่ประมวลผลพร้อมกัน

## 🤝 การสนับสนุน

- 📚 Documentation: `help(package = "TidyChunking")`
- 🐛 Bug reports: [GitHub Issues](https://github.com/datakruroo/TidyChunking/issues)
- 💡 Feature requests: [GitHub Discussions](https://github.com/datakruroo/TidyChunking/discussions)

## 📄 License

MIT License - ดู [LICENSE](LICENSE) สำหรับรายละเอียด

---

**TidyChunking** - Smart Markdown Chunking for Text Analysis 🚀 
