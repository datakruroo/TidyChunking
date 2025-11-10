# TidyChunking Demo Script
# ========================

library(TidyChunking)

# Sample Thai markdown content about data literacy
markdown_text <- "
# TidyChunking Demo Script - Advanced Customization
# ==================================================

library(TidyChunking)

# Sample English markdown content about data literacy
markdown_text <- "# Chapter 1: Introduction to Data Literacy

## 1.1 Definition and Concepts

Data literacy is the ability to read, understand, analyze and use data effectively. In the digital age, this skill is essential for professionals in all fields.

The importance of data literacy in the modern world can be summarized as follows:
- Helps in making data-driven decisions
- Increases competitiveness of individuals and organizations  
- Reduces risk from wrong decisions

## 1.2 Components of Data Literacy

### 1.2.1 Reading and Understanding Data

Data reading is an important basic skill that includes:
- Interpreting numbers and graphs
- Identifying trends and patterns
- Comparing data from multiple sources

### 1.2.2 Data Analysis

Analysis skills help us to:
- Find relationships between variables
- Predict future trends
- Create meaningful conclusions

# Chapter 2: Tools and Techniques

## 2.1 Data Analysis Tools

### 2.1.1 Microsoft Excel

Excel is a widely used basic tool with capabilities in:
- Managing and organizing data
- Creating graphs and charts
- Basic statistical calculations

### 2.1.2 R Programming

R is a specialized programming language for data analysis with strengths in:
- Managing large datasets
- Creating beautiful and high-quality graphs"

print("=== TidyChunking Advanced Demo ===")

# Step 1: Basic chunking
chunks <- chunk_for_keyword_extraction(markdown_text)
print(paste("Total chunks created:", nrow(chunks)))

# Step 2: Filter chunks with keywords
keyword_chunks <- filter_chunks_for_keywords(chunks)
print(paste("Keyword chunks:", nrow(keyword_chunks)))

# ========================================
# Part 1: Default Usage (Teacher Competencies)
# ========================================

if (require(tidyllm) && require(jsonlite)) {
  
  # Check API setup
  if (check_openai_setup()$status == "success") {
    
    print("\n=== Default: Teacher Data Literacy Competencies ===")
    
    # Default extraction - teacher competencies with default prompt and schema
    teacher_competencies <- extract_competencies_tidyllm(
      keyword_chunks, 
      max_per_chunk = 5
    )
    
    if (nrow(teacher_competencies) > 0) {
      print(teacher_competencies[1:min(3, nrow(teacher_competencies)), 
                               c("term", "category", "importance")])
      print("Category distribution:")
      print(table(teacher_competencies$category))
    }
    
    # ========================================
    # Part 2: Different Model
    # ========================================
    
    print("\n=== Using Different Model ===")
    
    # Use GPT-3.5-turbo instead of default gpt-4o-mini
    teacher_competencies_gpt35 <- extract_competencies_tidyllm(
      keyword_chunks[1:2, ],  # Just first 2 chunks for demo
      max_per_chunk = 3,
      model = "gpt-3.5-turbo"
    )
    
    if (nrow(teacher_competencies_gpt35) > 0) {
      print("Results with GPT-3.5-turbo:")
      print(teacher_competencies_gpt35[, c("term", "category")])
    }
    
    # ========================================
    # Part 3: Custom Business Prompt
    # ========================================
    
    print("\n=== Custom Business Skills Prompt ===")
    
    # Custom prompt function for business skills
    business_prompt <- function(n_comp, hier, text) {
      paste0(
        'Extract ', n_comp, ' KEY BUSINESS SKILLS from this text.\n\n',
        'Focus on practical workplace abilities.\n\n',
        'Categories: technical, analytical, communication, leadership, strategic\n',
        'Levels: critical, important, useful\n\n',
        'JSON format:\n',
        '[\n',
        '  {\n',
        '    "skill": "skill name",\n',
        '    "category": "technical",\n',
        '    "level": "critical",\n',
        '    "description": "brief description"\n',
        '  }\n',
        ']\n\n',
        'Text: ', text
      )
    }
    
    # Custom schema for business skills
    business_schema <- tidyllm::tidyllm_schema(
      name = "business_skills",
      competencies = tidyllm::field_object(
        .vector = TRUE,
        skill = tidyllm::field_chr(.description = "Business skill"),
        category = tidyllm::field_fct(.levels = c("technical", "analytical", "communication", "leadership", "strategic")),
        level = tidyllm::field_fct(.levels = c("critical", "important", "useful")),
        description = tidyllm::field_chr(.description = "Skill description")
      )
    )
    
    # Extract with custom prompt and schema
    business_skills <- extract_competencies_tidyllm(
      keyword_chunks[1:2, ],
      max_per_chunk = 3,
      model = "gpt-4o-mini",
      custom_prompt = business_prompt,
      custom_schema = business_schema
    )
    
    if (nrow(business_skills) > 0) {
      print("Business skills extracted:")
      print(business_skills[, c("skill", "category", "level")])
    }
    
    # ========================================
    # Part 4: Simple Research Skills Prompt
    # ========================================
    
    print("\n=== Custom Research Skills Prompt ===")
    
    # Simple research prompt
    research_prompt <- function(n_comp, hier, text) {
      paste0(
        'Find ', n_comp, ' RESEARCH SKILLS in this text.\n',
        'Categories: methodology, analysis, writing, technology\n',
        'JSON: [{"skill": "name", "type": "methodology"}]\n\n',
        'Text: ', text
      )
    }
    
    # Simple schema
    research_schema <- tidyllm::tidyllm_schema(
      name = "research",
      competencies = tidyllm::field_object(
        .vector = TRUE,
        skill = tidyllm::field_chr(.description = "Research skill"),
        type = tidyllm::field_fct(.levels = c("methodology", "analysis", "writing", "technology"))
      )
    )
    
    research_skills <- extract_competencies_tidyllm(
      keyword_chunks[1, , drop = FALSE],  # Just one chunk
      max_per_chunk = 2,
      custom_prompt = research_prompt,
      custom_schema = research_schema
    )
    
    if (nrow(research_skills) > 0) {
      print("Research skills:")
      print(research_skills[, c("skill", "type")])
    }
    
    print("\n=== Demo completed successfully! ===")
    print("You can now customize extract_competencies_tidyllm for your specific needs:")
    print("- Change the model parameter")
    print("- Write custom prompt functions")  
    print("- Define custom schemas for your domain")
    
  } else {
    print("OpenAI API not configured. Please set OPENAI_API_KEY in .Renviron")
  }
} else {
  print("Please install required packages: install.packages(c('tidyllm', 'jsonlite'))")
}

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
cat("To extract teacher competencies for data-driven classroom with AI:\n")
cat("1. Set up your OpenAI API key in .Renviron:\n")
cat("   usethis::edit_r_environ()\n")
cat("   Add: OPENAI_API_KEY=\"your-key-here\"\n")
cat("2. Restart R session\n")
cat("3. Install packages: install.packages(c('tidyllm', 'jsonlite'))\n")
cat("4. Use: teacher_competencies <- extract_competencies_tidyllm(keyword_chunks)\n")
cat("5. Validate: validated <- validate_competencies(teacher_competencies, chunks)\n")
cat("\nNote: New version focuses on extracting terms that actually exist in your text,\n")
cat("      reducing AI hallucination and ensuring grounded results!\n")