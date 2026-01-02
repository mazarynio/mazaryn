defmodule Mazaryn.Learning.CertificateGenerator do
  require Logger

  def generate_certificate(user_name, course_title, completion_date, certificate_id) do
    html = build_certificate_html(user_name, course_title, completion_date, certificate_id)

    case generate_pdf_from_html(html) do
      {:ok, pdf_binary} -> {:ok, pdf_binary}
      {:error, reason} -> {:error, reason}
    end
  end

  defp build_certificate_html(user_name, course_title, completion_date, certificate_id) do
    """
    <!DOCTYPE html>
    <html>
    <head>
      <meta charset="UTF-8">
      <style>
        @import url('https://fonts.googleapis.com/css2?family=Playfair+Display:wght@400;700&family=Montserrat:wght@300;400;600&display=swap');

        * {
          margin: 0;
          padding: 0;
          box-sizing: border-box;
        }

        body {
          font-family: 'Montserrat', sans-serif;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          padding: 60px;
          width: 1200px;
          height: 900px;
        }

        .certificate {
          background: white;
          width: 100%;
          height: 100%;
          padding: 80px;
          position: relative;
          box-shadow: 0 20px 60px rgba(0,0,0,0.3);
          border: 20px solid #f8f9fa;
          border-image: linear-gradient(135deg, #667eea 0%, #764ba2 100%) 1;
        }

        .border-pattern {
          position: absolute;
          top: 40px;
          left: 40px;
          right: 40px;
          bottom: 40px;
          border: 3px solid #667eea;
          pointer-events: none;
        }

        .header {
          text-align: center;
          margin-bottom: 60px;
          position: relative;
          z-index: 1;
        }

        .logo {
          font-size: 48px;
          font-weight: 700;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
          margin-bottom: 20px;
          font-family: 'Playfair Display', serif;
        }

        .title {
          font-size: 64px;
          font-weight: 700;
          color: #2d3748;
          margin-bottom: 20px;
          font-family: 'Playfair Display', serif;
          letter-spacing: 2px;
        }

        .subtitle {
          font-size: 24px;
          color: #718096;
          font-weight: 300;
          letter-spacing: 4px;
          text-transform: uppercase;
        }

        .content {
          text-align: center;
          margin: 60px 0;
          position: relative;
          z-index: 1;
        }

        .presented-to {
          font-size: 20px;
          color: #718096;
          margin-bottom: 20px;
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: 2px;
        }

        .recipient-name {
          font-size: 72px;
          font-weight: 700;
          color: #667eea;
          margin-bottom: 40px;
          font-family: 'Playfair Display', serif;
        }

        .completion-text {
          font-size: 22px;
          color: #4a5568;
          line-height: 1.8;
          margin-bottom: 30px;
          max-width: 800px;
          margin-left: auto;
          margin-right: auto;
        }

        .course-title {
          font-size: 36px;
          font-weight: 700;
          color: #2d3748;
          margin: 30px 0;
          font-family: 'Playfair Display', serif;
        }

        .footer {
          display: flex;
          justify-content: space-between;
          align-items: flex-end;
          margin-top: 80px;
          padding-top: 40px;
          border-top: 2px solid #e2e8f0;
          position: relative;
          z-index: 1;
        }

        .date-section, .signature-section {
          text-align: center;
          flex: 1;
        }

        .signature-line {
          border-top: 2px solid #2d3748;
          width: 300px;
          margin: 0 auto 15px;
        }

        .label {
          font-size: 16px;
          color: #718096;
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: 1px;
        }

        .value {
          font-size: 20px;
          color: #2d3748;
          font-weight: 600;
          margin-top: 8px;
        }

        .certificate-id {
          position: absolute;
          bottom: 20px;
          right: 20px;
          font-size: 12px;
          color: #a0aec0;
          font-family: 'Courier New', monospace;
        }

        .seal {
          position: absolute;
          bottom: 80px;
          left: 80px;
          width: 120px;
          height: 120px;
          border-radius: 50%;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          display: flex;
          align-items: center;
          justify-content: center;
          box-shadow: 0 8px 20px rgba(102, 126, 234, 0.4);
        }

        .seal-inner {
          width: 100px;
          height: 100px;
          border-radius: 50%;
          border: 3px solid white;
          display: flex;
          align-items: center;
          justify-content: center;
          color: white;
          font-size: 14px;
          font-weight: 600;
          text-align: center;
          padding: 10px;
        }
      </style>
    </head>
    <body>
      <div class="certificate">
        <div class="border-pattern"></div>

        <div class="header">
          <div class="logo">MAZARYN</div>
          <div class="title">Certificate</div>
          <div class="subtitle">of Achievement</div>
        </div>

        <div class="content">
          <div class="presented-to">This is to certify that</div>
          <div class="recipient-name">#{user_name}</div>

          <div class="completion-text">
            has successfully completed the course
          </div>

          <div class="course-title">#{course_title}</div>

          <div class="completion-text">
            demonstrating exceptional dedication and mastery of the subject matter
          </div>
        </div>

        <div class="footer">
          <div class="date-section">
            <div class="label">Date of Completion</div>
            <div class="value">#{completion_date}</div>
          </div>

          <div class="signature-section">
            <div class="signature-line"></div>
            <div class="label">Instructor Signature</div>
          </div>
        </div>

        <div class="seal">
          <div class="seal-inner">
            VERIFIED
          </div>
        </div>

        <div class="certificate-id">Certificate ID: #{certificate_id}</div>
      </div>
    </body>
    </html>
    """
  end

  defp generate_pdf_from_html(html) do
    try do
      System.cmd(
        "wkhtmltopdf",
        [
          "--page-size",
          "A4",
          "--orientation",
          "Landscape",
          "--margin-top",
          "0",
          "--margin-bottom",
          "0",
          "--margin-left",
          "0",
          "--margin-right",
          "0",
          "--enable-local-file-access",
          "-",
          "-"
        ], input: html, stderr_to_stdout: true)
      |> case do
        {pdf_binary, 0} -> {:ok, pdf_binary}
        {error, _} -> {:error, "PDF generation failed: #{error}"}
      end
    rescue
      error ->
        Logger.error("PDF generation error: #{inspect(error)}")
        {:error, :pdf_generation_failed}
    end
  end

  def generate_certificate_id do
    timestamp = :os.system_time(:millisecond)
    random = :crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower)
    "CERT-#{timestamp}-#{random}"
  end

  def verify_certificate(certificate_id) do
    try do
      case :learningdb.get_certificate(certificate_id) do
        {:ok, cert} -> {:ok, cert}
        _ -> {:error, :not_found}
      end
    rescue
      _ -> {:error, :verification_failed}
    end
  end
end
